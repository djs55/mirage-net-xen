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
  features_available: features;
} with sexp

type frontend_configuration = {
  tx_ring_ref: int;
  rx_ring_ref: int;
  event_channel: int;
  feature_requests: features;
} with sexp

  val description: string
  (** Human-readable description suitable for help text or
      a manpage *)
end

module type ENDPOINT = sig
  type t with sexp_of
  (** Type of a vchan endpoint. *)

  type port with sexp_of
  (** Type of a vchan port name. *)

  type error = [
    `Unknown of string
  ]

  val server :
    domid:int ->
    port:port ->
    ?read_size:int ->
    ?write_size:int ->
    unit -> t Lwt.t

  val client :
    domid:int ->
    port:port ->
    unit -> t Lwt.t

  val close : t -> unit Lwt.t
  (** Close a vchan. This deallocates the vchan and attempts to free
      its resources. The other side is notified of the close, but can
      still read any data pending prior to the close. *)

  include V1_LWT.FLOW
    with type flow = t
    and  type error := error
    and  type 'a io = 'a Lwt.t
    and  type buffer = Cstruct.t

  val description: string
  (** Human-readable description suitable for help text or
      a manpage *)
end
