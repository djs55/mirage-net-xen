(*
 * Copyright (c) 2010-2013 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014-2015 Citrix Inc
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
open Sexplib.Std
open Lwt
open Printf

module Make(E: Evtchn.S.EVENTS
  with type 'a io = 'a Lwt.t
)(M: Memory.S.MEMORY
)(C: S.CONFIGURATION
  with type 'a io = 'a Lwt.t) = struct

module Ring = Shared_memory_ring.Rpc.Make(E)(M)

type 'a io = 'a Lwt.t
type page_aligned_buffer = Cstruct.t
type buffer = Cstruct.t
type macaddr = Macaddr.t

(** IO operation errors *)
type error = [
| `Unknown of string (** an undiagnosed error *)
| `Unimplemented     (** operation not yet implemented in the code *)
| `Disconnected      (** the device has been previously disconnected *)
]

let allocate_ring ~domid =
  M.share ~domid ~npages:1 ~rw:true ~contents:`Zero ()
  >>= fun share ->
  let x = M.buf_of_share share in
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

end

type id = [
| `Client of int (* device id *)
| `Server of int * int (* domid * device id *)
]

type stats = {
  mutable rx_bytes : int64;
  mutable rx_pkts : int32;
  mutable tx_bytes : int64;
  mutable tx_pkts : int32;
}

type tx_ring = [
| `Front of (TX.response,int) Ring.Front.t
| `Back  of (TX.response,int) Ring.Back.t
]

type rx_ring = [
| `Front of (RX.response,int) Ring.Front.t
| `Back  of (RX.response,int) Ring.Back.t
]

type transport = {
  id: id;
  backend_id: int;
  backend: string;
  mac: Macaddr.t;
  tx_ring: tx_ring;
  tx_mutex: Lwt_mutex.t; (* Held to avoid signalling between fragments *)
  mutable tx_next_id: int;
  rx_ring: rx_ring;
  (* we share batches of pages with the backend and unshare
     when the last id is replied to. An entry in this array with
     index i means i is the last id using the share. *)
  rx_shares: M.share option array;
  mutable rx_next_id: int;
  (* The granted page corresponding to each slot *)
  rx_pages: Cstruct.t array;
  channel: E.channel;
  features: S.features;
  stats : stats;
}

type t = {
  mutable t: transport;
  mutable resume_fns: (t -> unit Lwt.t) list;
  mutable receive_callback: Cstruct.t -> unit Lwt.t;
  l : Lwt_mutex.t;
  c : unit Lwt_condition.t;
}

type t' = string with sexp_of
let sexp_of_t _ = sexp_of_t' "Netchannel.Endpoint.t"


let id t = t.t.id
let backend_id t = t.t.backend_id

let devices : (id, t) Hashtbl.t = Hashtbl.create 1

let features = {
  S.rx_copy = true;
  rx_notify = true;
  sg = true;
  (* FIXME: not sure about these *)
  rx_flip = false;
  gso_tcpv4 = false;
  smart_poll = false;
}

let plug_client id =
  let name = Sexplib.Sexp.to_string (S.sexp_of_id id) in
  C.read_backend id
  >>= fun b ->
  C.read_mac id
  >>= fun mac ->
  E.listen b.S.backend_id
  >>= fun (port, channel) ->

  allocate_ring ~domid:b.S.backend_id
  >>= fun (rx_gnt, buf) ->
  let rx_ring = Ring.Front.init ~buf ~idx_size:RX.Proto_64.total_size
    ~name:("Netchannel.RX." ^ name) channel string_of_int in

  allocate_ring ~domid:b.S.backend_id
  >>= fun (tx_gnt, buf) ->
  let tx_ring = Ring.Front.init ~buf ~idx_size:TX.Proto_64.total_size
    ~name:("Netchannel.TX." ^ name) channel string_of_int in

  let frontend = {
    S.tx_ring_ref = M.int32_of_grant tx_gnt;
    rx_ring_ref = M.int32_of_grant rx_gnt;
    event_channel = E.string_of_port port;
    feature_requests = features;
  } in
  C.write_frontend_configuration id frontend
  >>= fun () ->
  C.connect id
  >>= fun () ->
  let rx_shares = Array.make (Ring.Front.nr_ents rx_ring) None in
  let tx_next_id = 0 in
  let rx_next_id = 0 in
  let rx_pages = Array.make (Ring.Front.nr_ents rx_ring) (Cstruct.create 0) in
  let stats = { rx_pkts=0l;rx_bytes=0L;tx_pkts=0l;tx_bytes=0L } in
  let tx_mutex = Lwt_mutex.create () in
  let backend_id = b.S.backend_id in
  let backend = b.S.backend in
  let rx_ring = `Front rx_ring in
  let tx_ring = `Front tx_ring in
  return { id; backend_id; tx_ring; tx_mutex; tx_next_id;
    rx_ring; rx_shares; rx_pages; rx_next_id; stats;
    channel; mac; backend; features;
  }

let plug_server id =
  let name = Sexplib.Sexp.to_string (S.sexp_of_id id) in
  C.write_backend id features
  >>= fun b ->
  C.read_mac id
  >>= fun mac ->
  C.read_frontend_configuration id
  >>= fun f ->
  C.connect id
  >>= fun () ->
  let port = match E.port_of_string f.S.event_channel with
  | `Ok x -> x
  | `Error x -> failwith x in
  E.connect b.S.frontend_id port
  >>= fun channel ->
  let tx_gnt = M.grant_of_int32 f.S.tx_ring_ref in
  M.map ~domid:b.S.backend_id ~grant:tx_gnt ~rw:true
  >>= fun mapping ->
  let buf = M.buf_of_mapping mapping in
  (* Flip TX and RX around *)
  let rx_ring = Ring.Back.init ~buf ~idx_size:RX.Proto_64.total_size
    ~name:("Netchannel.RX." ^ name) channel string_of_int in
  let rx_gnt = M.grant_of_int32 f.S.rx_ring_ref in
  M.map ~domid:b.S.backend_id ~grant:rx_gnt ~rw:true
  >>= fun mapping ->
  let buf = M.buf_of_mapping mapping in
  let tx_ring = Ring.Back.init ~buf ~idx_size:RX.Proto_64.total_size
    ~name:("Netchannel.TX." ^ name) channel string_of_int in
  let rx_shares = Array.make (Ring.Back.nr_ents rx_ring) None in
  let tx_next_id = 0 in
  let rx_next_id = 0 in
  let rx_pages = Array.make (Ring.Back.nr_ents rx_ring) (Cstruct.create 0) in
  let stats = { rx_pkts=0l;rx_bytes=0L;tx_pkts=0l;tx_bytes=0L } in
  let tx_mutex = Lwt_mutex.create () in
  let backend_id = b.S.backend_id in
  let backend = b.S.backend in
  let rx_ring = `Back rx_ring in
  let tx_ring = `Back tx_ring in
  return { id; backend_id; tx_ring; tx_mutex; tx_next_id;
    rx_ring; rx_shares; rx_pages; rx_next_id; stats;
    channel; mac; backend; features;
  }

let plug id =
  Printf.printf "Netchannel.plug: id=%s\n%!"
    (Sexplib.Sexp.to_string (S.sexp_of_id id));
  match id with
  | `Client _ -> plug_client id
  | `Server _ -> plug_server id

(** Set of active block devices *)
let devices : (id, t) Hashtbl.t = Hashtbl.create 1

let devices_waiters : (id, t Lwt.u Lwt_sequence.t) Hashtbl.t = Hashtbl.create 1

let rec to_pages remaining =
  if Cstruct.len remaining <= 4096
  then [ remaining ]
  else Cstruct.sub remaining 0 4096 :: (to_pages (Cstruct.shift remaining 4096))


let refill_requests nf =
  let num = Ring.Front.get_free_requests nf.rx_ring in
  if num > 0 then
    M.share ~domid:nf.backend_id ~npages:num ~rw:true ~contents:`Zero ()
    >>= fun share ->
    let pages = to_pages (M.buf_of_share share) in
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
         nf.rx_pages.(!last_id) <- page;
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
  if Hashtbl.mem devices id then
    return (`Ok (Hashtbl.find devices id))
  else begin
    let id' = Sexplib.Sexp.to_string (S.sexp_of_id id) in
    printf "Netif.connect %s\n%!" id';
    try_lwt
      lwt t = plug id in
      let l = Lwt_mutex.create () in
      let c = Lwt_condition.create () in
      (* packets are dropped until listen is called *)
      let receive_callback = fun _ -> return () in
      let dev = { t; resume_fns=[]; receive_callback; l; c } in
      let (_: unit Lwt.t) = poll_thread dev in
      Hashtbl.add devices id dev;
      return (`Ok dev)
    with exn ->
      return (`Error (`Unknown (Printexc.to_string exn)))
  end

(* Unplug shouldn't block, although the Xen one might need to due
   to Xenstore? XXX *)
let disconnect t =
  printf "Netif: disconnect\n%!";
  Hashtbl.remove devices t.t.id;
  return ()

let page_size = 4096

(* Push a single page to the ring, but no event notification *)
let write_request ?size ~flags nf page =
  let len = Cstruct.len page in
  if page.Cstruct.off + len > page_size then begin
    (* netback rejects packets that cross page boundaries *)
    let msg =
      Printf.sprintf "Invalid page: offset=%d, length=%d" page.Cstruct.off len in
    print_endline msg;
    Lwt.fail (Failure msg)
  end else begin
    M.share ~domid:nf.t.backend_id ~npages:1 ~rw:false ~contents:(`Buffer page) ()
    >>= fun share ->
    let grants = M.grants_of_share share in
    (* we've already checked that page is a single page, therefore 1 grant *)
    let gref = List.hd grants in
    let size = match size with |None -> len |Some s -> s in
    (* XXX: another place where we peek inside the cstruct *)
    nf.t.stats.tx_pkts <- Int32.succ nf.t.stats.tx_pkts;
    nf.t.stats.tx_bytes <- Int64.add nf.t.stats.tx_bytes (Int64.of_int size);
    let offset = page.Cstruct.off in
    let id = nf.t.tx_next_id in
    nf.t.tx_next_id <- (nf.t.tx_next_id + 1) mod (1 lsl 16);
    let gref = M.int32_of_grant gref in
    Ring.Front.write nf.t.tx_ring (TX.Proto_64.write ~id ~gref ~offset ~flags ~size)
    >>= fun replied_t ->
    (* return a thread waiting for the reply which unshares the page *)
    return
      (Lwt.catch
        (fun () ->
          replied_t
          >>= fun _ ->
          M.unshare share
        ) (function
          | Shared_memory_ring.Rpc.Shutdown ->
            M.unshare share
            >>= fun () ->
            fail Shared_memory_ring.Rpc.Shutdown
          | e ->
            M.unshare share
            >>= fun () ->
            fail e
        )
      )
  end

(* Transmit a packet from buffer, with offset and length *)
let rec write_already_locked nf page =
  Lwt.catch
    (fun () ->
      write_request ~flags:0 nf page
      >>= fun th ->
      Ring.Front.push nf.t.tx_ring
      >>= fun () ->
      th
      (* all fragments acknowledged, resources cleaned up *)
    ) (function
      | Shared_memory_ring.Rpc.Shutdown ->
        write_already_locked nf page
      | e -> fail e)

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
         let numfree = Ring.Front.get_free_requests nf.t.tx_ring in
         if n >= numfree then
           E.recv nf.t.channel event
           >>= fun event ->
           wait_for_free_tx event n
         else
           return ()
       in
       let numneeded = List.length pages in
       wait_for_free_tx E.initial numneeded
       >>= fun () ->
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
         write_request ~flags:TX.Proto_64.flag_more_data ~size nf first_page
         >>= fun first_th ->
         let rec xmit = function
           | [] -> return []
           | hd :: [] ->
             write_request ~flags:0 nf hd
             >>= fun th ->
             return [ th ]
           | hd :: tl ->
             write_request ~flags:TX.Proto_64.flag_more_data nf hd
             >>= fun next_th ->
             xmit tl
             >>= fun rest ->
             return (next_th :: rest) in
         xmit other_pages
         >>= fun rest_th ->
         (* All fragments are now written, we can now notify the backend *)
         Ring.Front.push nf.t.tx_ring
    )

let listen nf fn =
  (* packets received from this point on will go to [fn]. Historical
     packets have not been stored: we don't want to buffer the network *)
  nf.receive_callback <- fn;
  let t, _ = Lwt.task () in
  t (* never return *)

let resume (id,t) =
  plug id
  >>= fun transport ->
  let old_transport = t.t in
  t.t <- transport;
  Lwt_list.iter_s (fun fn -> fn t) t.resume_fns
  >>= fun () ->
  Lwt_mutex.with_lock t.l
      (fun () -> Lwt_condition.broadcast t.c (); return ())
  >>= fun () ->
  Ring.Front.shutdown old_transport.rx_ring;
  Ring.Front.shutdown old_transport.tx_ring;
  return ()

let resume () =
  let devs = Hashtbl.fold (fun k v acc -> (k,v)::acc) devices [] in
  Lwt_list.iter_p (fun (k,v) -> resume (k,v)) devs

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

end
