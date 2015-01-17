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

module Make
  (E: Evtchn.S.EVENTS with type 'a io = 'a Lwt.t)
  (M: Memory.S.MEMORY) : sig

  type t with sexp_of
  (** An active frontend connection *)

  val make: S.backend_configuration -> t Lwt.t
  (** Create a connected frontend from the given backend configuration. Packets
      will be dropped until [listen] is called. *)

  val listen: t -> (Cstruct.t -> unit Lwt.t) -> unit Lwt.t
  (** [listen t callback] registers the [callback] function which will be called
      on receipt of packets in future. Returns a thread which never terminates. *)

  val write: t -> Cstruct.t -> unit Lwt.t
  (** [write t buffer] transmits a single packet with contents
      [buffer]. *)

  val writev: t -> Cstruct.t list -> unit Lwt.t
  (** [writev t buffers] transmits a single packet with contents
      taken by concatenating [buffers] *)

  val stats: t -> Stats.t
  (** [stats t] returns packet counter stats for [t] *)

end
