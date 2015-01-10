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
open Lwt

module Net = Netchannel.Endpoint.Make
  (Inheap_events)
  (Inheap_memory)
  (Netchannel_inheap_configuration)

let (>>|=) m f = m >>= function
| `Ok x -> f x
| `Error (`Unknown x) -> fail (Failure x)
| `Error `Disconnected -> fail (Failure "Disconnected")
| `Error `Unimplemented -> fail (Failure "Unimplemented")
| `Eof -> fail (Failure "EOF")

let with_connection f =
  let server_t = Net.connect (`Server(0, 2)) in
  let client_t = Net.connect (`Client 2) in

  server_t >>|= fun server ->
  client_t >>|= fun client ->
  let shutdown () =
    Net.disconnect client >>= fun () ->
    Net.disconnect server >>= fun () ->
    return () in
  Lwt.catch
    (fun () ->
      f client server >>= fun x ->
      shutdown () >>= fun () ->
      return x
    ) (fun e ->
      Printf.fprintf stderr "client = %s\n%!" (Sexplib.Sexp.to_string_hum (Net.sexp_of_t client));
      Printf.fprintf stderr "server = %s\n%!" (Sexplib.Sexp.to_string_hum (Net.sexp_of_t server));
      shutdown () >>= fun () ->
      fail e
    )

let assert_cleaned_up () =
  Inheap_memory.assert_cleaned_up ();
  Inheap_events.assert_cleaned_up ()


let cstruct_of_string s =
  let cstr = Cstruct.create (String.length s) in
  Cstruct.blit_from_string s 0 cstr 0 (String.length s);
  cstr
let string_of_cstruct c = String.escaped (Cstruct.to_string c)

open OUnit

let test_write_read =
  "write then read"
  >:: (fun () ->
    Lwt_main.run (
      with_connection
        (fun client server ->
          let state = ref `Initial in
          let c = Lwt_condition.create () in
          Net.listen client
            (fun buf ->
              let str = string_of_cstruct buf in
              ( state := match !state, str with
                | `Initial, "hello" -> `Second
                | `Initial, x ->
                  Printf.fprintf stderr "In initial state, read: %s\n%!" x;
                  `Failed
                | `Second, "vchan world" -> `Completed
                | `Second, x ->
                  Printf.fprintf stderr "In second state, read: %s\n%!" x;
                  `Failed
                | s, _ -> s );
              Lwt_condition.signal c ();
              return ()
            ) >>= fun () ->
          Net.write server (cstruct_of_string "hello") >>= fun () ->
          let rec loop () = match !state with
          | `Second -> return ()
          | `Initial ->
            Lwt_condition.wait c >>= fun () ->
            loop ()
          | _ -> failwith "Unexpected state" in
          loop () >>= fun () ->
          Net.write client (cstruct_of_string "vchan world") >>= fun () ->
          let rec loop () = match !state with
          | `Completed -> return ()
          | `Seond ->
            Lwt_condition.wait c >>= fun () ->
            loop ()
          | _ -> failwith "Unexpected state" in
          loop () >>= fun () ->
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
