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

  module F = Frontend.Make(E)(M)
  module B = Backend.Make(E)(M)

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

  type id = [
  | `Client of int (* device id *)
  | `Server of int * int (* domid * device id *)
  ] with sexp

  type implementation =
  | Frontend of F.t
  | Backend of B.t
  with sexp_of

  type t = {
    id: id;
    mac: Macaddr.t;
    implementation: implementation;
  } with sexp_of


  let id t = t.id

  (** Set of active block devices *)
  let devices : (id, t) Hashtbl.t = Hashtbl.create 1

  let devices_waiters : (id, t Lwt.u Lwt_sequence.t) Hashtbl.t = Hashtbl.create 1

  let connect id =
    if Hashtbl.mem devices id then
      return (`Ok (Hashtbl.find devices id))
    else begin
      let id' = Sexplib.Sexp.to_string (S.sexp_of_id id) in
      printf "Netif.Endpoint.(E)(M)(C).connect %s\n%!" id';
      Lwt.catch
        (fun () ->
          C.read_mac id
          >>= fun mac ->
          ( match id with
            | `Client _ ->
              fail (Failure "not implemented")
            | `Server (_, _) ->
              fail (Failure "not implemented")
          ) >>= fun implementation ->
          let t = { id; implementation; mac } in
          Hashtbl.add devices id t;
          return (`Ok t)
        ) (fun exn ->
          return (`Error (`Unknown (Printexc.to_string exn)))
        )
    end

  (* Unplug shouldn't block, although the Xen one might need to due
     to Xenstore? XXX *)
  let disconnect t =
    printf "Netif: disconnect\n%!";
    Hashtbl.remove devices t.id;
    return ()

  type stats = {
    mutable rx_bytes : int64;
    mutable rx_pkts : int32;
    mutable tx_bytes : int64;
    mutable tx_pkts : int32;
  }

  let resume t = fail (Failure "resume not implemented")
  let reset_stats_counters t =
    let stats = match t.implementation with
    | Frontend f -> F.stats f
    | Backend b -> B.stats b in
    Stats.reset stats
  let get_stats_counters t =
    let stats = match t.implementation with
    | Frontend f -> F.stats f
    | Backend b -> B.stats b in
    { rx_bytes = Int64.of_int stats.Stats.rx_bytes;
      rx_pkts  = Int32.of_int stats.Stats.rx_pkts;
      tx_bytes = Int64.of_int stats.Stats.tx_bytes;
      tx_pkts  = Int32.of_int stats.Stats.tx_pkts }
  let mac t = t.mac
  let listen t fn = fail (Failure "listen not implemented")
  let writev t bufs = fail (Failure "writev not implemented")
  let write t buf = fail (Failure "write not implemented")

end
