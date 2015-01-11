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
open Lwt
open Sexplib.Std

module Make(Xs: Xs_client_lwt.S) = struct

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

  let read_int x =
    try
      return (int_of_string x)
    with _ ->
      fail (Failure (Printf.sprintf "Expected an integer: %s" x))

  (* Return the path of the frontend *)
  let frontend = function
  | `Client devid ->
    return (Printf.sprintf "device/vif/%d/" devid)
  | `Server (domid, devid) ->
    Xs.make ()
    >>= fun xsc ->
    Xs.(immediate xsc (fun h -> read h (Printf.sprintf "backend/vif/%d/%d/backend" domid devid)))

  let backend = function
  | `Client devid ->
    Xs.make ()
    >>= fun xsc ->
    Xs.(immediate xsc (fun h -> read h (Printf.sprintf "device/vif/%d/backend" devid)))
  | `Server (domid, devid) ->
    return (Printf.sprintf "backend/vif/%d/%d" domid devid)

  let read_mac id =
    frontend id
    >>= fun frontend ->
    Xs.make ()
    >>= fun xsc ->
    Xs.(immediate xsc (fun h -> read h (frontend ^ "mac")))
    >|= Macaddr.of_string
    >>= function
    | Some x -> return x
    | None ->
      let m = Macaddr.make_local (fun _ -> Random.int 255) in
      Printf.printf "Netfront %s: no configured MAC, using %s"
        (Sexplib.Sexp.to_string (S.sexp_of_id id)) (Macaddr.to_string m);
      return m

  let write_frontend_configuration id (f: frontend_configuration) =
    frontend id
    >>= fun frontend ->
    Xs.make ()
    >>= fun xsc ->
    Xs.(transaction xsc (fun h ->
      let wrfn k v = write h (frontend ^ k) v in
      let write_feature k v =
        wrfn ("feature-" ^ k) (if v then "1" else "0") in
      wrfn "tx-ring-ref" (Int32.to_string f.tx_ring_ref) >>= fun () ->
      wrfn "rx-ring-ref" (Int32.to_string f.rx_ring_ref) >>= fun () ->
      wrfn "event-channel" f.event_channel >>= fun () ->
      write_feature "rx-copy" f.feature_requests.rx_copy >>= fun () ->
      write_feature "rx-notify" f.feature_requests.rx_notify >>= fun () ->
      write_feature "sg" f.feature_requests.sg >>= fun () ->
      (* XXX: rx-flip, smart-poll, gso-tcpv4 *)
      return ()
    ))

  let connect id =
    Xs.make ()
    >>= fun xsc ->
    Xs.(immediate xsc (fun h ->
      ( match id with
        | `Client _ -> frontend id
        | `Server (_, _) -> backend id )
      >>= fun path ->
      write h (path ^ "state") "4"
    ))

  let write_backend id features =
    backend id
    >>= fun backend ->
    Xs.make ()
    >>= fun xsc ->
    Xs.(immediate xsc (fun h ->
      let write_feature k v =
        write h (Printf.sprintf "%s/feature-%s" backend k) (if v then "1" else "0") in
      write_feature "sg" features.sg
      >>= fun () ->
      write_feature "gso-tcpv4" features.gso_tcpv4
      >>= fun () ->
      write_feature "rx-copy" features.rx_copy
      >>= fun () ->
      write_feature "rx-flip" features.rx_flip
      >>= fun () ->
      write_feature "rx-notify" features.rx_notify
      >>= fun () ->
      write_feature "smart-poll" features.smart_poll
      >>= fun () ->
      frontend id
      >>= fun frontend ->
      read h (frontend ^ "backend-id")
      >>= fun backend_id ->
      read_int backend_id
      >>= fun backend_id ->
      read h (frontend ^ "backend")
      >>= fun backend ->
      return { backend; backend_id; features_available = features }
    ))

  let read_backend id =
    frontend id
    >>= fun frontend ->
    Xs.make ()
    >>= fun xsc ->
    Xs.(immediate xsc (fun h ->
      read h (frontend ^ "backend-id")
      >>= fun backend_id ->
      read_int backend_id
      >>= fun backend_id ->
      read h (frontend ^ "backend")
      >>= fun backend ->
      let read_feature k =
        Lwt.catch
          (fun () ->
            read h (Printf.sprintf "%s/feature-%s" backend k)
            >>= fun v ->
            return (v = "1"))
          (fun _ -> return false) in
       read_feature "sg"
       >>= fun sg ->
       read_feature "gso-tcpv4"
       >>= fun gso_tcpv4 ->
       read_feature "rx-copy"
       >>= fun rx_copy ->
       read_feature "rx-flip"
       >>= fun rx_flip ->
       read_feature "rx-notify"
       >>= fun rx_notify ->
       read_feature "smart-poll"
       >>= fun smart_poll ->
       let features_available = { sg; gso_tcpv4; rx_copy; rx_flip; rx_notify; smart_poll } in
       return { backend; backend_id; features_available }
   ))

  let description = "Configuration information will be shared via Xenstore keys"

end
