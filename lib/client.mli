(*
 * Copyright (c) 2011-2013 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014 Citrix Inc
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

(** Xen Netfront interface for Ethernet I/O. *)

module Make
  (E: Evtchn.S.EVENTS
    with type 'a io = 'a Lwt.t)
  (M: Memory.S.MEMORY)
  (C: S.CONFIGURATION
    with type 'a io = 'a Lwt.t) : sig

  include V1.NETWORK
    with type 'a io = 'a Lwt.t
     and type     page_aligned_buffer = Cstruct.t
     and type     buffer = Cstruct.t
     and type     id = string
     and type     macaddr = Macaddr.t

  val resume: unit -> unit Lwt.t
  (** Call this to trigger a reconnection, needed after a resume *)
end
