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
open Netchannel
open Sexplib.Std
open Lwt

type 'a io = 'a Lwt.t

let _env_var = "NETCHANNEL_CONFIGURATION"

let read_mac _ = return (Macaddr.make_local (fun _ -> Random.int 255))

let write_frontend_configuration id t =
  Printf.fprintf stderr "%s=\"%s\"; export %s\n%!" _env_var (String.escaped (Sexplib.Sexp.to_string_hum (S.sexp_of_frontend_configuration t))) _env_var;
  return ()

let read_frontend_configuration id =
  try
    return (S.frontend_configuration_of_sexp (Sexplib.Sexp.of_string (Sys.getenv _env_var)))
  with Not_found ->
    Printf.fprintf stderr "Failed to find %s in the process environment\n%!" _env_var;
    fail Not_found

let connect id =
  Printf.fprintf stderr "Connected\n%!";
  return ()

let write_backend id features =
  let frontend_id = 0 in
  let backend_id = 0 in
  let backend = "" in
  let features_available = features in
  let t = { S.frontend_id; backend_id; backend; features_available } in
  Printf.fprintf stderr "%s=\"%s\"; export %s\n%!" _env_var (String.escaped (Sexplib.Sexp.to_string_hum (S.sexp_of_backend_configuration t))) _env_var;
  return t

let read_backend id =
  try
    return (S.backend_configuration_of_sexp (Sexplib.Sexp.of_string (Sys.getenv _env_var)))
  with Not_found ->
    Printf.fprintf stderr "Failed to find %s in the process environment\n%!" _env_var;
    fail Not_found

    let description = "Configuration information will be shared via Unix environment variables."
