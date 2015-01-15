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

type t =
| Checksum_blank (* 1 *)
| Data_validated (* 2 *)
| More_data      (* 4 *)
| Extra_info     (* 8 *)
with sexp

let flags = [
  Checksum_blank, 1;
  Data_validated, 2;
  More_data,      4;
  Extra_info,     8;
]

let unmarshal x =
  match List.fold_left (fun (residual, flags) (flag, bit) ->
    if residual land bit <> 0
    then (residual lxor bit, flag :: flags)
    else (residual, flags)
  ) (x, []) flags with
   | 0, flags -> `Ok flags
  | x, _ -> `Error (Printf.sprintf "Unknown bit while parsing flags: %d" x)

let marshal = List.fold_left (fun acc flag -> acc lor (List.mem_assoc flag flags)) 0
