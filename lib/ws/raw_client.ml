open Eio.Std

type t =
  { wsd : Httpun_ws.Wsd.t;
    incoming : Frames.incoming Eio.Stream.t;
    runtime : Gluten_eio_flow.Client.t;
    (* Set once [`Eof] has been taken from [incoming]; only late messages
       already queued behind it remain to be drained. *)
    mutable draining : bool
  }

let sha1 s = Digestif.SHA1.(digest_string s |> to_raw_string)
let random_nonce () = String.init 16 (fun _ -> Char.chr (Random.int 256))

let show_error : Httpun_ws.Client_connection.error -> string = function
  | `Handshake_failure (response, _body) ->
    Format.asprintf "handshake failure: %a" Httpun.Response.pp_hum response
  | `Malformed_response reason -> "malformed response: " ^ reason
  | `Invalid_response_body_length _ -> "invalid response body length"
  | `Exn exn -> Printexc.to_string exn
;;

let endpoint_of_uri uri =
  let scheme = Uri.scheme uri |> Option.value ~default:"ws" in
  let tls =
    match scheme with
    | "wss" | "https" -> true
    | _ -> false
  in
  let host =
    match Uri.host uri with
    | Some host -> host
    | None -> invalid_arg "websocket url without host"
  in
  let port = Uri.port uri |> Option.value ~default:(if tls then 443 else 80) in
  let resource =
    match Uri.path_and_query uri with
    | "" -> "/"
    | resource -> resource
  in
  (tls, host, port, resource)
;;

let connect_flow ~sw ~net ~tls ~host ~port =
  let addresses = Eio.Net.getaddrinfo_stream net host ~service:(string_of_int port) in
  let address =
    match addresses with
    | address :: _ -> address
    | [] -> failwith (Printf.sprintf "cannot resolve %s:%d" host port)
  in
  let socket = Eio.Net.connect ~sw net address in
  if tls
  then
    (Net_support.wrap_tls
       ~host
       (socket :> [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] r)
      :> [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] r)
  else (socket :> [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] r)
;;

let connect ~sw ~env uri =
  let net = Eio.Stdenv.net env in
  let tls, host, port, resource = endpoint_of_uri uri in
  let flow = connect_flow ~sw ~net ~tls ~host ~port in
  let incoming = Frames.create_incoming () in
  let connected, resolve_connected = Promise.create () in
  let websocket_handler wsd =
    Promise.resolve resolve_connected (Ok wsd);
    Frames.handlers ~wsd incoming
  in
  let error_handler error =
    if Promise.is_resolved connected
    then Eio.Stream.add incoming `Eof
    else Promise.resolve resolve_connected (Error (show_error error))
  in
  let connection =
    Httpun_ws.Client_connection.connect
      ~nonce:(random_nonce ())
      ~headers:(Httpun.Headers.of_list [ ("host", host ^ ":" ^ string_of_int port) ])
      ~sha1
      ~error_handler
      ~websocket_handler
      resource
  in
  let runtime =
    Gluten_eio_flow.Client.create
      ~sw
      ~read_buffer_size:0x1000
      ~protocol:(module Httpun_ws.Client_connection)
      connection
      flow
  in
  match Promise.await connected with
  | Ok wsd -> { wsd; incoming; runtime; draining = false }
  | Error reason -> failwith ("websocket connection failed: " ^ reason)
;;

(* Raises [End_of_file] once the connection is gone.

   httpun-ws can notify the connection-level eof *before* delivering frames
   that were already buffered (e.g. a final message the peer sent right
   before closing). Those deliveries happen synchronously before this
   consumer fiber wakes up, so when we take [`Eof] any late messages are
   already queued behind it: keep draining them before reporting eof. *)
let receive_one t =
  let take_late () =
    match Eio.Stream.take_nonblocking t.incoming with
    | Some (`Msg message) -> message
    | Some `Eof | None -> raise End_of_file
  in
  if t.draining
  then take_late ()
  else (
    match Eio.Stream.take t.incoming with
    | `Msg message -> message
    | `Eof ->
      t.draining <- true;
      take_late ())
;;

(* Sending on a connection the peer is closing is a no-op, not an error: the
   reader will surface [`Eof] shortly. *)
let send_one t message =
  if not (Httpun_ws.Wsd.is_closed t.wsd)
  then (
    (* [send_bytes] masks (mutates) the buffer on clients, so copy. *)
    let payload = Bytes.of_string message in
    try
      Httpun_ws.Wsd.send_bytes
        t.wsd
        ~kind:`Text
        payload
        ~off:0
        ~len:(Bytes.length payload)
    with
    | Failure _ -> ())
;;

let close t =
  if not (Httpun_ws.Wsd.is_closed t.wsd) then Httpun_ws.Wsd.close t.wsd;
  ignore (Gluten_eio_flow.Client.shutdown t.runtime : unit Promise.t)
;;
