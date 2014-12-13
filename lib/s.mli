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

module type CONFIGURATION = sig

  type 'a io

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

  val read_mac: int -> Macaddr.t io

  val write_frontend_configuration: int -> frontend_configuration -> unit io

  val connect: int -> unit io

  val read_backend: int -> backend_configuration io

  val description: string
  (** Human-readable description suitable for help text or
      a manpage *)
end
