(*
 * Copyright (c) 2010-2013 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt
open Printf

type 'a io = 'a Lwt.t
type page_aligned_buffer = Io_page.t
type buffer = Cstruct.t
type macaddr = Macaddr.t

(** IO operation errors *)
type error = [
  | `Unknown of string (** an undiagnosed error *)
  | `Unimplemented     (** operation not yet implemented in the code *)
  | `Disconnected      (** the device has been previously disconnected *)
]

module Make(E: Evtchn.S.EVENTS
  with type 'a io = 'a Lwt.t
)(M: Memory.S.MEMORY
)(C: S.CONFIGURATION
  with type 'a io = 'a Lwt.t) = struct

module Ring = Shared_memory_ring.Rpc.Make(E)(M)

let allocate_ring ~domid =
  M.share ~domid ~npages:1 ~rw:true
  >>= fun share ->
  let x = Io_page.to_cstruct (M.buf_of_share share) in
  (* npages = 1 ==> List.length grants = 1 *)
  let gnt = List.hd (M.grants_of_share share) in
  for i = 0 to Cstruct.len x - 1 do
    Cstruct.set_uint8 x i 0
  done;
  return (gnt, x)

module RX = struct

  module Proto_64 = struct
    cstruct req {
      uint16_t       id;
      uint16_t       _padding;
      uint32_t       gref
    } as little_endian

    let write ~id ~gref slot =
      set_req_id slot id;
      set_req_gref slot gref;
      id

        cstruct resp {
        uint16_t       id;
        uint16_t       offset;
        uint16_t       flags;
        uint16_t       status
      } as little_endian

    let read slot =
      get_resp_id slot,
      (get_resp_offset slot, get_resp_flags slot, get_resp_status slot)

    let total_size = max sizeof_req sizeof_resp
    let _ = assert(total_size = 8)
  end

  type response = int * int * int

  let create (id, domid, channel) =
    let name = sprintf "Netif.RX.%d" id in
    allocate_ring ~domid
    >>= fun (rx_gnt, buf) ->
    let ring = Ring.Front.init ~buf ~idx_size:Proto_64.total_size ~name channel string_of_int in
    return (rx_gnt, ring)

end

module TX = struct

  type response = int

  module Proto_64 = struct
    cstruct req {
      uint32_t       gref;
      uint16_t       offset;
      uint16_t       flags;
      uint16_t       id;
      uint16_t       size
    } as little_endian

    type flags =
      |Checksum_blank (* 1 *)
      |Data_validated (* 2 *)
      |More_data      (* 4 *)
      |Extra_info     (* 8 *)

    let flag_more_data = 4

    let write ~gref ~offset ~flags ~id ~size slot =
      set_req_gref slot gref;
      set_req_offset slot offset;
      set_req_flags slot flags;
      set_req_id slot id;
      set_req_size slot size;
      id

        cstruct resp {
        uint16_t       id;
        uint16_t       status
      } as little_endian

    let read slot =
      get_resp_id slot, get_resp_status slot

    let total_size = max sizeof_req sizeof_resp
    let _ = assert(total_size = 12)
  end

  let create (id, domid, channel) =
    let name = sprintf "Netif.TX.%d" id in
    allocate_ring ~domid
    >>= fun (rx_gnt, buf) ->
    let ring = Ring.Front.init ~buf ~idx_size:Proto_64.total_size ~name channel string_of_int in
    return (rx_gnt, ring)
end

type stats = {
  mutable rx_bytes : int64;
  mutable rx_pkts : int32;
  mutable tx_bytes : int64;
  mutable tx_pkts : int32;
}

type transport = {
  id: int;
  backend_id: int;
  backend: string;
  mac: Macaddr.t;
  tx_ring: (TX.response,int) Ring.Front.t;
  tx_gnt: M.grant;
  tx_mutex: Lwt_mutex.t; (* Held to avoid signalling between fragments *)
  rx_ring: (RX.response,int) Ring.Front.t;
  (* we share batches of pages with the backend and unshare
     when the last id is replied to. An entry in this array with
     index i means i is the last id using the share. *)
  rx_shares: M.share option array;
  mutable rx_next_id: int;
  (* The granted page corresponding to each slot *)
  rx_pages: Cstruct.t array;
  rx_gnt: M.grant;
  channel: E.channel;
  features: C.features;
  stats : stats;
}

type t = {
  mutable t: transport;
  mutable resume_fns: (t -> unit Lwt.t) list;
  mutable receive_callback: Cstruct.t -> unit Lwt.t;
  l : Lwt_mutex.t;
  c : unit Lwt_condition.t;
}

type id = string

let id t = string_of_int t.t.id
let backend_id t = t.t.backend_id

let devices : (id, t) Hashtbl.t = Hashtbl.create 1

(* Given a VIF ID and backend domid, construct a netfront record for it *)
let plug_inner id =
  let tx_mutex = Lwt_mutex.create () in

  C.read_backend id
  >>= fun b ->
  let backend_id = b.C.backend_id in
  let backend = b.C.backend in
  C.read_mac id
  >>= fun mac ->
  Printf.printf "Netfront.create: id=%d domid=%d mac=%s\n%!"
    id backend_id (Macaddr.to_string mac);

  (* Allocate a transmit and receive ring, and event channel for them *)
  E.listen b.C.backend_id
  >>= fun (port, channel) ->
  RX.create (id, b.C.backend_id, channel)
  >>= fun (rx_gnt, rx_ring) ->
  TX.create (id, b.C.backend_id, channel)
  >>= fun (tx_gnt, tx_ring) ->

  let features = {
    C.rx_copy = true;
    rx_notify = true;
    sg = true;
    (* FIXME: not sure about these *)
    rx_flip = false;
    gso_tcpv4 = false;
    smart_poll = false;
  } in
  let frontend = {
    C.tx_ring_ref = M.int32_of_grant tx_gnt;
    rx_ring_ref = M.int32_of_grant rx_gnt;
    event_channel = E.string_of_port port;
    feature_requests = features;
  } in
  C.write_frontend_configuration id frontend
  >>= fun () ->
  C.connect id
  >>= fun () ->
  let rx_shares = Array.make (Ring.Front.nr_ents rx_ring) None in
  let rx_next_id = 0 in
  let rx_pages = Array.make (Ring.Front.nr_ents rx_ring) (Cstruct.create 0) in
  let stats = { rx_pkts=0l;rx_bytes=0L;tx_pkts=0l;tx_bytes=0L } in
  (* Register callback activation *)
  return { id; backend_id; tx_ring; tx_gnt; tx_mutex;
           rx_gnt; rx_ring; rx_shares; rx_pages; rx_next_id; stats;
           channel; mac; backend; features;
         }

(** Set of active block devices *)
let devices : (int, t) Hashtbl.t = Hashtbl.create 1

let devices_waiters : (int, t Lwt.u Lwt_sequence.t) Hashtbl.t = Hashtbl.create 1

let refill_requests nf =
  let num = Ring.Front.get_free_requests nf.rx_ring in
  if num > 0 then
    M.share ~domid:nf.backend_id ~npages:num ~rw:true
    >>= fun share ->
    let pages = Io_page.to_pages (M.buf_of_share share) in
    let grants = M.grants_of_share share in
    (* work out the id of the last page in the share, associate
       the mapping with this. We will bulk-unmap when this slot
       is acked. Note the whole set of buffers will remain allocated
       until this happens so there's no danger of granting them to
       two domains at once. *)
    let last_id = ref 0 in
    assert(grants <> []);
    List.iter
      (fun (gref', page) ->
         let slot_id = Ring.Front.next_req_id nf.rx_ring in
         let slot = Ring.Front.slot nf.rx_ring slot_id in

         let gref = M.int32_of_grant gref' in
         last_id := nf.rx_next_id;
         nf.rx_next_id <- (nf.rx_next_id + 1) mod (Array.length nf.rx_shares  );
         nf.rx_pages.(!last_id) <- Io_page.to_cstruct page;
         ignore(RX.Proto_64.write ~id:(!last_id) ~gref slot)
      ) (List.combine grants pages);
    assert(nf.rx_shares.(!last_id) = None);
    nf.rx_shares.(!last_id) <- Some share;

    if Ring.Front.push_requests_and_check_notify nf.rx_ring
    then E.send nf.channel
    else return ()
  else return ()

let rx_poll nf (fn: Cstruct.t -> unit Lwt.t) =
  Ring.Front.ack_responses nf.rx_ring (fun slot ->
      let id,(offset,flags,status) = RX.Proto_64.read slot in

      (* unshare the data so the grant table indices can be reused *)
      (match nf.rx_shares.(id) with
      | Some share ->
        nf.rx_shares.(id) <- None;
        ignore_result (M.unshare share)
      | None -> ());

      match status with
      |sz when status > 0 ->
        let page = nf.rx_pages.(id) in
        let packet = Cstruct.sub page 0 sz in
        nf.stats.rx_pkts <- Int32.succ nf.stats.rx_pkts;
        nf.stats.rx_bytes <- Int64.add nf.stats.rx_bytes (Int64.of_int sz);
        ignore_result
          (try_lwt fn packet
           with exn -> return (printf "RX exn %s\n%!" (Printexc.to_string exn)))
      |err -> printf "RX error %d\n%!" err
    )

let tx_poll nf =
  Ring.Front.poll nf.tx_ring TX.Proto_64.read

let poll_thread (nf: t) : unit Lwt.t =
  let rec loop from =
    refill_requests nf.t
    >>= fun () ->
    rx_poll nf.t nf.receive_callback;
    tx_poll nf.t;

    E.recv nf.t.channel from
    >>= fun from ->
    loop from in
  loop E.initial

let connect id =
  (* id must match the xenstore entry *)
  match (try Some (int_of_string id) with _ -> None) with
  | Some id' -> begin
      if Hashtbl.mem devices id' then
        return (`Ok (Hashtbl.find devices id'))
      else begin
        printf "Netif.connect %d\n%!" id';
        try_lwt
          lwt t = plug_inner id' in
          let l = Lwt_mutex.create () in
          let c = Lwt_condition.create () in
          (* packets are dropped until listen is called *)
          let receive_callback = fun _ -> return () in
          let dev = { t; resume_fns=[]; receive_callback; l; c } in
          let (_: unit Lwt.t) = poll_thread dev in
          Hashtbl.add devices id' dev;
          return (`Ok dev)
        with exn ->
          return (`Error (`Unknown (Printexc.to_string exn)))
      end
    end
  | None ->
    printf "Netif.connect %s: could not find device\n" id;
    return (`Error (`Unknown (Printf.sprintf "device %s not found" id)))

(* Unplug shouldn't block, although the Xen one might need to due
   to Xenstore? XXX *)
let disconnect t =
  printf "Netif: disconnect\n%!";
  Hashtbl.remove devices t.t.id;
  return ()

let page_size = Io_page.round_to_page_size 1

(* Push a single page to the ring, but no event notification *)
let write_request ?size ~flags nf page =
  let len = Cstruct.len page in
  if page.Cstruct.off + len > page_size then begin
    (* netback rejects packets that cross page boundaries *)
    let msg =
      Printf.sprintf "Invalid page: offset=%d, length=%d" page.Cstruct.off len in
    print_endline msg;
    Lwt.fail (Failure msg)
  end else
  lwt gref = Gnt.Gntshr.get () in
  (* This grants access to the *base* data pointer of the page *)
  (* XXX: another place where we peek inside the cstruct *)
  Gnt.Gntshr.grant_access ~domid:nf.t.backend_id ~writable:false gref page.Cstruct.buffer;
  let size = match size with |None -> len |Some s -> s in
  (* XXX: another place where we peek inside the cstruct *)
  nf.t.stats.tx_pkts <- Int32.succ nf.t.stats.tx_pkts;
  nf.t.stats.tx_bytes <- Int64.add nf.t.stats.tx_bytes (Int64.of_int size);
  let offset = page.Cstruct.off in
  lwt replied = Lwt_ring.Front.write nf.t.tx_client
      (TX.Proto_64.write ~id:gref ~gref:(Int32.of_int gref) ~offset ~flags ~size) in
  (* request has been written; when replied returns we have a reply *)
  let replied =
    try_lwt
      lwt _ = replied in
      Gnt.Gntshr.end_access gref;
      Gnt.Gntshr.put gref;
      return ()
    with Lwt_ring.Shutdown ->
      Gnt.Gntshr.put gref;
      fail Lwt_ring.Shutdown
       | e ->
         Gnt.Gntshr.end_access gref;
         Gnt.Gntshr.put gref;
         fail e in
  return replied

(* Transmit a packet from buffer, with offset and length *)
let rec write_already_locked nf page =
  try_lwt
    lwt th = write_request ~flags:0 nf page in
    Lwt_ring.Front.push nf.t.tx_client (notify nf.t);
    lwt () = th in
    (* all fragments acknowledged, resources cleaned up *)
    return ()
  with | Lwt_ring.Shutdown -> write_already_locked nf page

let write nf page =
  Lwt_mutex.with_lock nf.t.tx_mutex
    (fun () ->
       write_already_locked nf page
    )

(* Transmit a packet from a list of pages *)
let writev nf pages =
  Lwt_mutex.with_lock nf.t.tx_mutex
    (fun () ->
       let rec wait_for_free_tx event n =
         let numfree = Ring.Rpc.Front.get_free_requests nf.t.tx_fring in
         if n >= numfree then
           lwt event = Activations.after nf.t.channel event in
           wait_for_free_tx event n
         else
           return ()
       in
       let numneeded = List.length pages in
       wait_for_free_tx Activations.program_start numneeded >>
       match pages with
       |[] -> return ()
       |[page] ->
         (* If there is only one page, then just write it normally *)
         write_already_locked nf page
       |first_page::other_pages ->
         (* For Xen Netfront, the first fragment contains the entire packet
          * length, which is the backend will use to consume the remaining
          * fragments until the full length is satisfied *)
         let size = Cstruct.lenv pages in
         lwt first_th =
           write_request ~flags:TX.Proto_64.flag_more_data ~size nf first_page in
         let rec xmit = function
           | [] -> return []
           | hd :: [] ->
             lwt th = write_request ~flags:0 nf hd in
             return [ th ]
           | hd :: tl ->
             lwt next_th = write_request ~flags:TX.Proto_64.flag_more_data nf hd in
             lwt rest = xmit tl in
             return (next_th :: rest) in
         lwt rest_th = xmit other_pages in
         (* All fragments are now written, we can now notify the backend *)
         Lwt_ring.Front.push nf.t.tx_client (notify nf.t);
         return ()
    )

let wait_for_plug nf =
  Printf.printf "Wait for plug...\n";
  Lwt_mutex.with_lock nf.l (fun () ->
      while_lwt not (Eventchn.is_valid nf.t.channel) do
        Lwt_condition.wait ~mutex:nf.l nf.c
      done)

let listen nf fn =
  (* packets received from this point on will go to [fn]. Historical
     packets have not been stored: we don't want to buffer the network *)
  nf.receive_callback <- fn;
  let t, _ = Lwt.task () in
  t (* never return *)

(** Return a list of valid VIFs *)
let enumerate () =
  Xs.make ()
  >>= fun xsc ->
  catch
    (fun () ->
       Xs.(immediate xsc
             (fun h -> directory h "device/vif"))
       >|= (List.map int_of_string) )
    (fun _ -> return [])

let resume (id,t) =
  lwt transport = plug_inner id in
  let old_transport = t.t in
  t.t <- transport;
  lwt () = Lwt_list.iter_s (fun fn -> fn t) t.resume_fns in
  lwt () = Lwt_mutex.with_lock t.l
      (fun () -> Lwt_condition.broadcast t.c (); return ()) in
  Lwt_ring.Front.shutdown old_transport.rx_client;
  Lwt_ring.Front.shutdown old_transport.tx_client;
  return ()

let resume () =
  let devs = Hashtbl.fold (fun k v acc -> (k,v)::acc) devices [] in
  Lwt_list.iter_p (fun (k,v) -> resume (k,v)) devs

let add_resume_hook t fn =
  t.resume_fns <- fn::t.resume_fns

(* Type of callback functions for [create]. *)
type callback = id -> t -> unit Lwt.t

(* The Xenstore MAC address is colon separated, very helpfully *)
let mac nf = nf.t.mac

let get_stats_counters t = t.t.stats

let reset_stats_counters t =
  t.t.stats.rx_bytes <- 0L;
  t.t.stats.rx_pkts  <- 0l;
  t.t.stats.tx_bytes <- 0L;
  t.t.stats.tx_pkts  <- 0l

let _ =
  printf "Netif: add resume hook\n%!";
  Sched.add_resume_hook resume

end
