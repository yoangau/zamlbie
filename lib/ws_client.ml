open Lwt.Infix
module WsLwt = Websocket_lwt_unix

type t =
  { ws : WsLwt.conn;
    close_sent : bool ref;
    rx : string Lwt_stream.t
  }

let send_close ?content ws close_sent =
  let open Websocket in
  WsLwt.write ws (Frame.create ~opcode:Close ?content ())
  >>= fun () ->
  close_sent := true;
  WsLwt.close_transport ws
;;

let send_message ws message =
  let open Websocket in
  WsLwt.write ws (Frame.create ~content:message ()) >>= fun _ -> Lwt.return_unit
;;

let make_rx ws close_sent =
  let open Websocket in
  let filter_out _ = Lwt.return_some `Ignore in
  let close_stream _ = Lwt.return_none in
  let handle_control_frame ws close_sent frame =
    let open Websocket.Frame in
    match frame.opcode with
    | Ping -> WsLwt.write ws (create ~opcode:Pong ()) >>= filter_out
    | Close ->
      (if !close_sent
       then Lwt.return_unit
       else if String.length frame.content >= 2
       then send_close ~content:(String.sub frame.content 0 2) ws close_sent
       else WsLwt.write ws (Frame.close 1000))
      >>= fun _ -> WsLwt.close_transport ws >>= close_stream
    | Pong -> filter_out ()
    | _ -> WsLwt.close_transport ws >>= close_stream
  in
  let read_frame ws close_sent =
    WsLwt.read ws
    >>= fun frame ->
    match frame.opcode with
    | Ping | Pong | Close -> handle_control_frame ws close_sent frame
    | Text | Binary -> Lwt.return_some (`Forward frame.content)
    | _ -> Lwt.return_some `Ignore
  in
  Lwt_stream.from (fun () -> read_frame ws close_sent)
  |> Lwt_stream.filter_map (function
    | `Forward content -> Some content
    | _ -> None)
;;

let attach_tx ws close_sent =
  Lwt_stream.map_s (function
    | Some `Leave -> send_close ws close_sent
    | Some (`Message message) -> send_message ws message
    | _ -> Lwt.return_unit)
;;

let make ws =
  let close_sent = ref false in
  { ws; close_sent; rx = make_rx ws close_sent }
;;

let connect uri : t Lwt.t =
  let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
  Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system
  >>= fun endp ->
  Conduit_lwt_unix.endp_to_client ~ctx endp
  >>= fun client -> WsLwt.connect ~ctx client uri >|= make
;;

let duplex t receive send =
  let sending = attach_tx t.ws t.close_sent (send ()) in
  let receiving = Lwt_stream.map_s receive t.rx in
  [ sending; receiving ]
  |> Lwt_stream.choose
  |> Lwt_stream.iter_s (fun _ -> Lwt.return ())
;;

let receive_one t = Lwt_stream.next t.rx
let send_one t message = send_message t.ws message
let close t = send_close t.ws t.close_sent
