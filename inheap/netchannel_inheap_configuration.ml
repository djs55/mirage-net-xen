(*
 * Copyright (c) 2013,2014 Citrix Systems Inc
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

type 'a io = 'a Lwt.t

type features = {
  rx_copy: bool;
  rx_flip: bool;
  rx_notify: bool;
  sg: bool;
  gso_tcpv4: bool;
  smart_poll: bool;
} with sexp

type backend_configuration = {
  backend_id: int;
  backend: string;
  features_available: features;
} with sexp

type frontend_configuration = {
  tx_ring_ref: int32;
  rx_ring_ref: int32;
  event_channel: string;
  feature_requests: features;
} with sexp

let read_mac _ = return (Macaddr.make_local (fun _ -> Random.int 255))

let fc = ref None
let fc_c = Lwt_condition.create ()

let bc = ref None
let bc_c = Lwt_condition.create ()

let write_frontend_configuration id t =
  fc := Some t;
  Lwt_condition.signal fc_c ();
  return ()

let connect id =
  Printf.fprintf stderr "Connected\n%!";
  return ()

let read_backend id =
  let rec loop () = match !bc with
  | None -> Lwt_condition.wait bc_c >>= fun () -> loop ()
  | Some bc -> return bc in
  loop ()

let description = "Configuration information will be shared via global bindings"
