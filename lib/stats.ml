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
  mutable rx_bytes : int;
  mutable rx_pkts  : int;
  mutable tx_bytes : int;
  mutable tx_pkts  : int;
} with sexp

let create () = { rx_pkts=0; rx_bytes=0; tx_pkts=0; tx_bytes=0 }

let rx t size =
  t.rx_pkts <- t.rx_pkts + 1;
  t.rx_bytes <- t.rx_bytes + size

let tx t size =
  t.tx_pkts <- t.tx_pkts + 1;
  t.tx_bytes <- t.tx_bytes + size

let reset t =
  t.rx_bytes <- 0;
  t.rx_pkts  <- 0;
  t.tx_bytes <- 0;
  t.tx_pkts  <- 0
