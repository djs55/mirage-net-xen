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
open Lwt

type 'a io = 'a Lwt.t

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

let _env_var = "NETCHANNEL_CONFIGURATION"

let read_mac _ = return (Macaddr.make_local (fun _ -> Random.int 255))

let write_frontend_configuration id t =
  Printf.fprintf stderr "%s=\"%s\"; export %s\n%!" _env_var (String.escaped (Sexplib.Sexp.to_string_hum (sexp_of_frontend_configuration t))) _env_var;
  return ()

let connect id =
  Printf.fprintf stderr "Connected\n%!";
  return ()

let read_backend id =
  try
    return (backend_configuration_of_sexp (Sexplib.Sexp.of_string (Sys.getenv _env_var)))
  with Not_found ->
    Printf.fprintf stderr "Failed to find %s in the process environment\n%!" _env_var;
    fail Not_found

    let description = "Configuration information will be shared via Unix environment variables."
