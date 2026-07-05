(* Client-side TLS support shared by the HTTP and websocket clients.

   Only the game client uses TLS (https/wss towards a hosted server); the
   game server always listens in plain text behind a TLS-terminating proxy,
   so everything here runs on a single domain. *)

(* The TLS stack needs an RNG; installing it is idempotent enough for our
   single-domain client, but guard it anyway. *)
let ensure_rng =
  let installed = Atomic.make false in
  fun () ->
    if not (Atomic.exchange installed true) then Mirage_crypto_rng_unix.use_default ()
;;

let tls_client_config () =
  ensure_rng ();
  match Ca_certs.authenticator () with
  | Error (`Msg msg) -> Error ("no CA certificates found: " ^ msg)
  | Ok authenticator ->
    (match Tls.Config.client ~authenticator () with
     | Ok config -> Ok config
     | Error (`Msg msg) -> Error ("invalid TLS configuration: " ^ msg))
;;

let host_of_string host =
  match Domain_name.of_string host with
  | Error _ -> None
  | Ok domain_name ->
    (match Domain_name.host domain_name with
     | Error _ -> None
     | Ok host -> Some host)
;;

(* Wrap an established TCP flow in a client-side TLS session. *)
let wrap_tls ?host flow =
  match tls_client_config () with
  | Error msg -> failwith msg
  | Ok config ->
    let host = Option.bind host host_of_string in
    Tls_eio.client_of_flow config ?host flow
;;
