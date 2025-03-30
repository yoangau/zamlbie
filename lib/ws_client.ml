open Lwt.Infix
open Websocket_lwt_unix

type t =
  { ws : Websocket_lwt_unix.conn;
    close_sent : bool ref;
    rx : string Lwt_stream.t
  }

let send_close ?content ws close_sent =
  let open Websocket in
  write ws (Frame.create ~opcode:Close ?content ())
  >>= fun () ->
  close_sent := true;
  Lwt.return_unit
;;

let send_message ws message =
  let open Websocket in
  write ws (Frame.create ~content:message ()) >>= fun _ -> Lwt.return_unit
;;

let make_rx ws close_sent =
  let open Websocket in
  let handle_control_frame ws close_sent frame =
    let open Websocket.Frame in
    match frame.opcode with
    | Ping ->
      Websocket_lwt_unix.write ws (create ~opcode:Pong ())
      >>= fun _ -> Lwt.return @@ Some `Ignore
    | Close ->
      (if !close_sent
       then Lwt.return_unit
       else if String.length frame.content >= 2
       then send_close ~content:(String.sub frame.content 0 2) ws close_sent
       else Websocket_lwt_unix.write ws (Frame.close 1000))
      >>= fun _ -> Websocket_lwt_unix.close_transport ws >>= fun _ -> Lwt.return_none
    | Pong -> Lwt.return @@ Some `Ignore
    | _ -> Websocket_lwt_unix.close_transport ws >>= fun _ -> Lwt.return_none
  in
  let handle_data_frame frame =
    match frame.Websocket.Frame.opcode with
    | Text | Binary -> Lwt.return_some (`Forward frame.content)
    | _ -> Lwt.return_none
  in
  let read_frame ws close_sent =
    Websocket_lwt_unix.read ws
    >>= function
    | frame
      when frame.Websocket.Frame.opcode = Ping
           || frame.opcode = Pong
           || frame.opcode = Close -> handle_control_frame ws close_sent frame
    | frame -> handle_data_frame frame
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
  Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system
  >>= fun endp ->
  let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
  Conduit_lwt_unix.endp_to_client ~ctx endp
  >>= fun client -> connect ~ctx client uri >|= make
;;

let async_duplex { ws; rx; close_sent } receive send =
  let push = send () |> attach_tx ws close_sent in
  [ push; Lwt_stream.map_s receive rx ]
  |> Lwt_stream.choose
  |> Lwt_stream.iter_s (fun _ -> Lwt.return ())
;;
