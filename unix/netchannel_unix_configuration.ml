(*
 * Copyright (C) Citrix Systems Inc.
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
open Netchannel

open Sexplib.Std
open Lwt

type t = {
  ring_ref: string;
  event_channel: string;
} with sexp

let _env_var = "XEN_CONFIGURATION"

let write ~client_domid ~port t =
  Printf.fprintf stderr "%s=\"%s\"; export %s\n%!" _env_var (String.escaped (Sexplib.Sexp.to_string_hum (sexp_of_t t))) _env_var;
  return ()

let read ~server_domid ~port =
  try
    return (t_of_sexp (Sexplib.Sexp.of_string (Sys.getenv _env_var)))
  with Not_found ->
    Printf.fprintf stderr "Failed to find %s in the process environment\n%!" _env_var;
    fail Not_found

let delete ~client_domid ~port =
  return ()

let description = "Configuration information will be shared via Unix environment variables."
