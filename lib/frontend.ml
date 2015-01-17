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

module Make
  (E: Evtchn.S.EVENTS with type 'a io = 'a Lwt.t)
  (M: Memory.S.MEMORY) = struct

  module Ring = Shared_memory_ring.Rpc.Make(E)(M)

  (* The frontend grants the rings to the backend. We ensure
     the ring has no pending data on it. *)
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

  type transport = {
    backend_id: int;
    backend: string;
    tx_ring: (TX.Response.t,int) Ring.Front.t;
    tx_mutex: Lwt_mutex.t; (* Held to avoid signalling between fragments *)
    mutable tx_next_id: int;
    rx_ring: (RX.Response.t,int) Ring.Front.t;
    (* we share batches of pages with the backend and unshare
       when the last id is replied to. An entry in this array with
       index i means i is the last id using the share. *)
    rx_shares: M.share option array;
    mutable rx_next_id: int;
    (* The granted page corresponding to each slot *)
    rx_pages: Cstruct.t array;
    channel: E.channel;
    features: Features.t;
    frontend: S.frontend_configuration;
    stats : Stats.t;
  }

  type t = {
    mutable transport: transport;
    mutable resume_fns: (t -> unit Lwt.t) list;
    mutable receive_callback: Cstruct.t -> unit Lwt.t;
    l : Lwt_mutex.t;
    c : unit Lwt_condition.t;
  }

  type t' = string with sexp_of
  let sexp_of_t _ = sexp_of_t' "Netchannel.Frontend.t"

  let page_size = 4096
  let rec to_pages x =
    if Cstruct.len x <= page_size
    then [ x ]
    else Cstruct.sub x 0 page_size :: (to_pages (Cstruct.shift x page_size))

  (* Ensure that the RX ring is fully populated with pages for the backend
     to use for incoming traffic. *)
  let refill_requests t =
    let num = Ring.Front.get_free_requests t.transport.rx_ring in
    if num = 0
    then return ()
    else
      M.share ~domid:t.transport.backend_id ~npages:num ~rw:true ~contents:`Zero ()
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
          let slot_id = Ring.Front.next_req_id t.transport.rx_ring in
          let slot = Ring.Front.slot t.transport.rx_ring slot_id in

          let gref = M.int32_of_grant gref' in
          last_id := t.transport.rx_next_id;
          t.transport.rx_next_id <- (t.transport.rx_next_id + 1) mod (Array.length t.transport.rx_shares  );
          t.transport.rx_pages.(!last_id) <- page;
          RX.Request.(write { id = !last_id; gref } slot);
        ) (List.combine grants pages);
      assert(t.transport.rx_shares.(!last_id) = None);
      t.transport.rx_shares.(!last_id) <- Some share;

      if Ring.Front.push_requests_and_check_notify t.transport.rx_ring
      then E.send t.transport.channel
      else return ()

  (* For all the incoming traffic, run the receive_callback in background
     threads (NB this appears to be in parallel i.e. out-of-order) and
     unshare the memory in a background thread.
     FIXME: all the background threads appear to be racing *)
  let rx_poll t =
    Ring.Front.ack_responses t.transport.rx_ring (fun slot ->
      let open RX.Response in
      match read slot with
      | `Error msg -> printf "Netif.Frontend.rx_poll failed to parse: %s\n%!" msg
      | `Ok { id; offset; flags; status } ->

        (* unshare the data so the grant table indices can be reused *)
        ( match t.transport.rx_shares.(id) with
        | Some share ->
          t.transport.rx_shares.(id) <- None;
          ignore_result (M.unshare share)
        | None -> () );

        ( match status with
        |sz when status > 0 ->
          let page = t.transport.rx_pages.(id) in
          let packet = Cstruct.sub page 0 sz in
          Stats.rx t.transport.stats sz;
          ignore_result
          (try_lwt t.receive_callback packet
           with exn -> return (printf "RX exn %s\n%!" (Printexc.to_string exn)))
        |err -> printf "RX error %d\n%!" err )
    )

  let poll_thread (t: t) : unit Lwt.t =
    let rec loop from =
      refill_requests t
      >>= fun () ->
      rx_poll t;
      Ring.Front.poll t.transport.tx_ring
        (fun slot ->
          let res = TX.Response.read slot in
          res.TX.Response.id, res);

      E.recv t.transport.channel from
      >>= fun from ->
      loop from in
    loop E.initial

  let make b =
    E.listen b.S.backend_id
    >>= fun (port, channel) ->

    allocate_ring ~domid:b.S.backend_id
    >>= fun (rx_gnt, buf) ->
    let rx_ring = Ring.Front.init ~buf ~idx_size:RX.total_size
      ~name:("Netchannel.RX " ^ b.S.backend) channel string_of_int in

    allocate_ring ~domid:b.S.backend_id
    >>= fun (tx_gnt, buf) ->
    let tx_ring = Ring.Front.init ~buf ~idx_size:TX.total_size
      ~name:("Netchannel.TX " ^ b.S.backend) channel string_of_int in

    let frontend = {
      S.tx_ring_ref = M.int32_of_grant tx_gnt;
      rx_ring_ref = M.int32_of_grant rx_gnt;
      event_channel = E.string_of_port port;
      feature_requests = Features.supported;
    } in
    let rx_shares = Array.make (Ring.Front.nr_ents rx_ring) None in
    let tx_next_id = 0 in
    let rx_next_id = 0 in
    let rx_pages = Array.make (Ring.Front.nr_ents rx_ring) (Cstruct.create 0) in
    let stats = Stats.create () in
    let tx_mutex = Lwt_mutex.create () in
    let backend_id = b.S.backend_id in
    let backend = b.S.backend in
    let features = Features.supported in
    let transport = { backend_id; tx_ring; tx_mutex; tx_next_id;
      rx_ring; rx_shares; rx_pages; rx_next_id; stats;
      channel; backend; features; frontend
    } in
    let l = Lwt_mutex.create () in
    let c = Lwt_condition.create () in
    (* packets are dropped until listen is called *)
    let receive_callback = fun _ -> return () in
    let t = { transport; resume_fns=[]; receive_callback; l; c } in
    let (_: unit Lwt.t) = poll_thread t in
    return t

  let listen t fn =
    (* packets received from this point on will go to [fn]. Historical
    packets have not been stored: we don't want to buffer the network *)
    t.receive_callback <- fn;
    let task, _ = Lwt.task () in
    task (* never return *)

  (* Push a single page to the ring, but no event notification *)
  let write_request ?size ~flags t page =
    let len = Cstruct.len page in
    if page.Cstruct.off + len > page_size then begin
      (* netback rejects packets that cross page boundaries *)
      let msg =
        Printf.sprintf "Invalid page: offset=%d, length=%d" page.Cstruct.off len in
      print_endline msg;
      Lwt.fail (Failure msg)
    end else begin
      M.share ~domid:t.transport.backend_id ~npages:1 ~rw:false ~contents:(`Buffer page) ()
      >>= fun share ->
      let grants = M.grants_of_share share in
      (* we've already checked that page is a single page, therefore 1 grant *)
      let gref = List.hd grants in
      let size = match size with |None -> len |Some s -> s in
      Stats.tx t.transport.stats size;
      (* XXX: another place where we peek inside the cstruct *)
      let offset = page.Cstruct.off in
      let id = t.transport.tx_next_id in
      t.transport.tx_next_id <- (t.transport.tx_next_id + 1) mod (1 lsl 16);
      let gref = M.int32_of_grant gref in
      let req = TX.Request.({ id; gref; offset; flags; size }) in
      Ring.Front.write t.transport.tx_ring
        (fun slot ->
          TX.Request.write req slot;
          id
        );
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
  let rec write_already_locked t page =
    Lwt.catch
      (fun () ->
        write_request ~flags:[] t page
        >>= fun th ->
        Ring.Front.push t.transport.tx_ring
        >>= fun () ->
        th
        (* all fragments acknowledged, resources cleaned up *)
      ) (function
        | Shared_memory_ring.Rpc.Shutdown ->
          write_already_locked t page
        | e -> fail e)

  let write t page =
    Lwt_mutex.with_lock t.transport.tx_mutex
      (fun () ->
        write_already_locked t page
      )

  (* Transmit a packet from a list of pages *)
  let writev t pages =
    Lwt_mutex.with_lock t.transport.tx_mutex
      (fun () ->
        let rec wait_for_free_tx event n =
          let numfree = Ring.Front.get_free_requests t.transport.tx_ring in
          if n >= numfree then
            E.recv t.transport.channel event
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
            write_already_locked t page
          |first_page::other_pages ->
            (* For Xen Netfront, the first fragment contains the entire packet
               length, which is the backend will use to consume the remaining
               fragments until the full length is satisfied *)
            let size = Cstruct.lenv pages in
            write_request ~flags:[ Flag.More_data ] ~size t first_page
            >>= fun first_th ->
            let rec xmit = function
            | [] -> return []
            | hd :: [] ->
            write_request ~flags:[] t hd
            >>= fun th ->
            return [ th ]
          | hd :: tl ->
            write_request ~flags:[ Flag.More_data ] t hd
            >>= fun next_th ->
            xmit tl
            >>= fun rest ->
            return (next_th :: rest) in
          xmit other_pages
          >>= fun rest_th ->
          (* All fragments are now written, we can now notify the backend *)
          Ring.Front.push t.transport.tx_ring
      )

  let stats t = t.transport.stats
end
