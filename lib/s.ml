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

type id = [
| `Client of int (* device id *)
| `Server of int * int (* domid * device id *)
] with sexp


type features = {
  rx_copy: bool;
  rx_flip: bool;
  rx_notify: bool;
  sg: bool;
  gso_tcpv4: bool;
  smart_poll: bool;
} with sexp

type backend_configuration = {
  frontend_id: int;
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

module type CONFIGURATION = sig

  type 'a io

  val read_mac: id -> Macaddr.t io

  val read_frontend_configuration: id -> frontend_configuration io
  (** Waits for the frontend configuration to become available
      and returns it. *)

  val write_frontend_configuration: id -> frontend_configuration -> unit io

  val connect: id -> unit io

  val read_backend: id -> backend_configuration io

  val write_backend: id -> features -> backend_configuration io

  val description: string
  (** Human-readable description suitable for help text or
      a manpage *)
end

module type ENDPOINT = sig
  include V1.NETWORK
    with type 'a io = 'a Lwt.t
     and type     page_aligned_buffer = Cstruct.t
     and type     buffer = Cstruct.t
     and type     id = id
     and type     macaddr = Macaddr.t

  val sexp_of_t: t -> Sexplib.Sexp.t

  (** An ENDPOINT allows network connections to be created, and packets
      to be exchanged in both directions.
      Note that the Xen network protocols distinguish between two roles:
      client (aka "netfront") and server (aka "netback"). *)

  val resume: unit -> unit Lwt.t
  (** Call this to trigger a reconnection, needed after a resume *)
end
