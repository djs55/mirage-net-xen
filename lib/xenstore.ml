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
  open S

  type 'a io = 'a Lwt.t

  let read_int x =
    try
      return (int_of_string x)
    with _ ->
      fail (Failure (Printf.sprintf "Expected an integer: %s" x))

  let read_int32 x =
    try
      return (Int32.of_string x)
    with _ ->
      fail (Failure (Printf.sprintf "Expected a 32-bit integer: %s" x))

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

  let read_features path =
    Xs.make ()
    >>= fun xsc ->
    Xs.(immediate xsc
      (fun h ->
        let read_feature key =
          Lwt.catch
            (fun () ->
              read h (Printf.sprintf "%s/feature-%s" path key)
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
        return { sg; gso_tcpv4; rx_copy; rx_flip; rx_notify; smart_poll }
    )
  )

  let write_features path features =
    Xs.make ()
    >>= fun xsc ->
    Xs.(immediate xsc
      (fun h ->
        let write_feature k v =
          write h (Printf.sprintf "%s/feature-%s" path k) (if v then "1" else "0") in
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
      )
    )

  let write_frontend_configuration id (f: frontend_configuration) =
    frontend id
    >>= fun frontend ->
    Xs.make ()
    >>= fun xsc ->
    Xs.(transaction xsc (fun h ->
      let wrfn k v = write h (frontend ^ k) v in
      wrfn "tx-ring-ref" (Int32.to_string f.tx_ring_ref) >>= fun () ->
      wrfn "rx-ring-ref" (Int32.to_string f.rx_ring_ref) >>= fun () ->
      wrfn "event-channel" f.event_channel >>= fun () ->
      write_features frontend f.feature_requests
    ))

  let read_frontend_configuration id =
    frontend id
    >>= fun frontend ->
    Xs.make ()
    >>= fun xsc ->
    Xs.wait xsc (fun h ->
      Lwt.catch
        (fun () ->
          Xs.read h (frontend ^ "state")
          >>= fun state ->
          if state = "3" || state = "4"
          then return ()
          else raise Xs_protocol.Eagain
        ) (function
          | Xs_protocol.Enoent _ -> fail Xs_protocol.Eagain
          | e -> fail e)
    ) >>= fun () ->
    Xs.(immediate xsc
      (fun h ->
        read h (frontend ^ "tx-ring-ref")
        >>= fun tx_ring_ref ->
        read_int32 tx_ring_ref
        >>= fun tx_ring_ref ->
        read h (frontend ^ "rx-ring-ref")
        >>= fun rx_ring_ref ->
        read_int32 rx_ring_ref
        >>= fun rx_ring_ref ->
        read h (frontend ^ "event-channel")
        >>= fun event_channel ->
        read_features frontend
        >>= fun feature_requests ->
        return { tx_ring_ref; rx_ring_ref; event_channel; feature_requests }
      )
    )

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
      read h (backend ^ "frontend-id")
      >>= fun frontend_id ->
      read_int frontend_id
      >>= fun frontend_id ->
      write_features backend features
      >>= fun () ->
      frontend id
      >>= fun frontend ->
      read h (frontend ^ "backend-id")
      >>= fun backend_id ->
      read_int backend_id
      >>= fun backend_id ->
      read h (frontend ^ "backend")
      >>= fun backend ->
      return { backend; frontend_id; backend_id; features_available = features }
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
      read h (backend ^ "frontend-id")
      >>= fun frontend_id ->
      read_int frontend_id
      >>= fun frontend_id ->
      read_features backend
      >>= fun features_available ->
      return { backend; frontend_id; backend_id; features_available }
   ))

  let description = "Configuration information will be shared via Xenstore keys"

end
