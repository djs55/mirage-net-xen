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

type t = {
  mutable rx_bytes : int; (** total payload bytes received *)
  mutable rx_pkts  : int; (** total packets received *)
  mutable tx_bytes : int; (** total payload bytes transmitted *)
  mutable tx_pkts  : int; (** total packets transmitted *)
} with sexp
(** Per-connection statistics *)

val create: unit -> t
(** [create ()] returns a fresh set of zeroed counters *)

val rx: t -> int -> unit
(** [rx t size] records that we received a packet of length [size] *)

val tx: t -> int -> unit
(** [tx t size records that we transmitted a packet of length [size] *)

val reset: t -> unit
(** [reset t] resets all packet counters in [t] to 0 *)
