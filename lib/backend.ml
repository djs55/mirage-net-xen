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

  type t = {
    channel: E.channel;
    backend_configuration: S.backend_configuration;
    rx_ring: (RX.Response.t,int) Ring.Back.t;
    (* Grants we can write into *)
    rx_reqs: RX.Request.t Lwt_sequence.t;
    tx_ring: (TX.Response.t,int) Ring.Back.t;
    mutable receive_callback: Cstruct.t -> unit Lwt.t;
    stats: Stats.t;
  }

  type t' = string with sexp_of
  let sexp_of_t _ = sexp_of_t' "Netif.Backend.t"

  let make backend_configuration f =
    let port = match E.port_of_string f.S.event_channel with
    | `Ok x -> x
    | `Error x -> failwith x in
    E.connect backend_configuration.S.frontend_id port
    >>= fun channel ->
    let tx_gnt = M.grant_of_int32 f.S.tx_ring_ref in
    M.map ~domid:backend_configuration.S.backend_id ~grant:tx_gnt ~rw:true
    >>= fun mapping ->
    let buf = M.buf_of_mapping mapping in
    (* Flip TX and RX around *)
    let rx_ring = Ring.Back.init ~buf ~idx_size:RX.total_size
      ~name:("Netif.Backend.RX." ^ backend_configuration.S.backend) channel string_of_int in
    let rx_gnt = M.grant_of_int32 f.S.rx_ring_ref in
    M.map ~domid:backend_configuration.S.backend_id ~grant:rx_gnt ~rw:true
    >>= fun mapping ->
    let buf = M.buf_of_mapping mapping in
    let tx_ring = Ring.Back.init ~buf ~idx_size:RX.total_size
      ~name:("Netif.Backend.TX." ^ backend_configuration.S.backend) channel string_of_int in
    let stats = Stats.create () in
    let receive_callback _ = return_unit in
    let rx_reqs = Lwt_sequence.create () in
    return { channel; backend_configuration; rx_ring; tx_ring; rx_reqs; stats; receive_callback }

  (* check for incoming requests on the TX ring *)
  let read_thread (t: t) : unit Lwt.t =
    let rec loop after =
      let q = ref [] in
      Ring.Back.ack_requests t.tx_ring
        (fun slot ->
          match TX.Request.read slot with
          | `Error msg -> printf "Netif.Backend.read_read TX has unparseable request: %s" msg
          | `Ok req ->
            (* update stats *)
            q := req :: !q
        );
      (* -- at this point the ring slots may be overwritten *)
      let grants = List.map (fun req ->
        t.backend_configuration.S.frontend_id,
        M.grant_of_int32 req.TX.Request.gref
      ) !q in
      M.mapv ~grants ~rw:false
      >>= fun readonly_mapping ->
      let _ = (* perform everything else in a background thread *)
        let buf = M.buf_of_mapping readonly_mapping in
        (* XXX: partition into packets *)
        t.receive_callback buf
        >>= fun () ->
        M.unmap readonly_mapping;
        let notify = Ring.Back.push_responses_and_check_notify t.tx_ring in
        if notify then E.send t.channel else return () in
      E.recv t.channel after
      >>= fun after ->
      loop after in
    loop E.initial

  let listen t fn =
    (* packets received from this point on will go to [fn]. Historical
    packets have not been stored: we don't want to buffer the network *)
    t.receive_callback <- fn;
    let task, _ = Lwt.task () in
    task (* never return *)

  (* We need [n] pages to send a packet to the frontend. The Ring.Back API
     gives us all the requests that are available at once. Since we may need
     fewer of this, stash them in the t.rx_reqs sequence. *)
  let get_n_grefs t n =
    let rec take seq = function
    | 0 -> []
    | n -> Lwt_sequence.take_l seq :: (take seq (n - 1)) in
    let rec loop after =
      let n' = Lwt_sequence.length t.rx_reqs in
      if n' >= n then return (take t.rx_reqs n)
      else begin
        Ring.Back.ack_requests t.rx_ring
          (fun slot ->
            let req = RX.Request.read slot in
            ignore(Lwt_sequence.add_r req t.rx_reqs)
          );
        if Lwt_sequence.length t.rx_reqs <> n'
        then loop after
        else
          E.recv t.channel after
          >>= fun after ->
          loop after
      end in
    loop E.initial

  let write t buf =
    (* wait for a slot to be granted on the RX ring *)
    get_n_grefs t 1
    >>= fun reqs ->
    let req = List.hd reqs in
    M.map ~domid:t.backend_configuration.S.frontend_id ~grant:(M.grant_of_int32 req.RX.Request.gref) ~rw:true
    >>= fun mapping ->
    let frontend_buf = M.buf_of_mapping mapping in
    Cstruct.blit buf 0 frontend_buf 0 (Cstruct.len buf);
    M.unmap mapping;
    let slot = Ring.Back.(slot t.rx_ring (next_res_id t.rx_ring)) in
    RX.Response.(write { id = req.RX.Request.id; offset = 0; flags = []; status = (Cstruct.len buf) }) slot;
    if Ring.Back.push_responses_and_check_notify t.rx_ring
    then E.send t.channel
    else return ()

  let writev t buf =
    (* write for slots to be granted on the RX ring *)
    failwith "writev"

  let stats t = t.stats
end
