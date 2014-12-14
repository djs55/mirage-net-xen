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

(* should these be merged into Endpoint like vchan? *)
module Netfront = Netchannel.Client.Make
  (Inheap_events)
  (Inheap_memory)
  (Netchannel_inheap_configuration)

module Netback = Netchannel.Server.Make
  (Inheap_events)
  (Inheap_memory)
  (Netchannel_inheap_configuration)

let with_connection f =
  let server_t = Netback.server ~domid:1 ~port () in
  let client_t = Netfront.client ~domid:0 ~port () in
  server_t >>= fun server ->
  client_t >>= fun client ->
  let shutdown () =
    Netfront.close client >>= fun () ->
    Netback.close server in
  Lwt.catch
    (fun () ->
      f client server >>= fun x ->
      shutdown () >>= fun () ->
      return x
    ) (fun e ->
      Printf.fprintf stderr "client = %s\n%!" (Sexplib.Sexp.to_string_hum (Netfront.sexp_of_t client));
      Printf.fprintf stderr "server = %s\n%!" (Sexplib.Sexp.to_string_hum (Netback.sexp_of_t server));
      shutdown () >>= fun () ->
      fail e
    )

let assert_cleaned_up () =
  Inheap_memory.assert_cleaned_up ();
  Inheap_events.assert_cleaned_up ()

let (>>|=) m f = m >>= function
| `Ok x -> f x
| `Error (`Unknown x) -> fail (Failure x)
| `Eof -> fail (Failure "EOF")

let cstruct_of_string s =
  let cstr = Cstruct.create (String.length s) in
  Cstruct.blit_from_string s 0 cstr 0 (String.length s);
  cstr
let string_of_cstruct c = String.escaped (Cstruct.to_string c)

open OUnit

let test_write_read () =
  "write then read"
  >:: (fun () ->
    Lwt_main.run (
      with_connection ()
        (fun client server ->
          Netfront.write server (cstruct_of_string "hello") >>|= fun () ->
          Netback.read client >>|= fun buf ->
          assert_equal ~printer:(fun x -> x) "hello" (string_of_cstruct buf);
          Netfront.write client (cstruct_of_string "vchan world") >>|= fun () ->
          Netback.read server >>|= fun buf ->
          assert_equal ~printer:(fun x -> x) "vchan world" (string_of_cstruct buf);
          return ()
        )
    );
    assert_cleaned_up ()
  )

let _ =

  let suite = "netchannel" >::: [
    test_write_read;
  ] in
  OUnit2.run_test_tt_main (OUnit.ounit2_of_ounit1 suite)
