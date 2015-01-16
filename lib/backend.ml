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

module Make
  (E: Evtchn.S.EVENTS with type 'a io = 'a Lwt.t)
  (M: Memory.S.MEMORY) = struct

  module Ring = Shared_memory_ring.Rpc.Make(E)(M)

  type t = {
    channel: E.channel;
    rx_ring: (RX.response,int) Ring.Back.t;
    tx_ring: (TX.response,int) Ring.Back.t;
    mutable receive_callback: Cstruct.t -> unit Lwt.t;
    stats: Stats.t;
  }

  let make b f =
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
    let stats = Stats.zero () in
    return { channel; rx_ring; tx_ring; stats }

  (* check for incoming requests on the TX ring *)
  let read_thread (t: t) : unit Lwt.t =
    let rec loop after =
      let q = ref [] in
      Ring.Back.ack_requests t.tx_ring
        (fun slot ->
          let req = TX.parse_req slot in
          (* update stats *)
          q := req :: !q;
        );
      (* -- at this point the ring slots may be overwritten *)
      let grants = grants_of_segments (Array.to_list segs) in
      maybe_mapv false grants
      >>= fun readonly_mapping ->
      let _ = (* perform everything else in a background thread *)
        failwith "XXX call listen with all the fragments";
        maybe_unmap readonly_mapping
        >>= fun () ->
        let notify = Ring.Rpc.Back.push_responses_and_check_notify t.tx_ring in
        if notify then E.send t.channel else return () in
      E.recv t.channel after
      >>= fun after ->
      loop after in
    loop E.initial

end
