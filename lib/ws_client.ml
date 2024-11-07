open Lwt.Infix
open Websocket_lwt_unix

let section = Lwt_log.Section.make "client"

let client uri receive send =
  let open Websocket in
  Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system
  >>= fun endp ->
  let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
  Conduit_lwt_unix.endp_to_client ~ctx endp
  >>= fun client ->
  connect ~ctx client uri
  >>= fun conn ->
  let close_sent = ref false in
  let react () =
    Websocket_lwt_unix.read conn
    >>= function
    | { Frame.opcode = Ping; _ } ->
      write conn (Frame.create ~opcode:Pong ()) >>= fun _ -> Lwt.return @@ Some ()
    | { opcode = Close; content; _ } ->
      (* Immediately echo and pass this last message to the user *)
      (if !close_sent
       then Lwt.return_unit
       else if String.length content >= 2
       then write conn (Frame.create ~opcode:Close ~content:(String.sub content 0 2) ())
       else write conn (Frame.close 1000))
      >>= fun _ -> Websocket_lwt_unix.close_transport conn >>= fun _ -> Lwt.return None
    | { opcode = Pong; _ } -> Lwt.return @@ Some ()
    | { opcode = Text; content; _ } | { opcode = Binary; content; _ } ->
      receive content >>= fun _ -> Lwt.return @@ Some ()
    | _ -> Websocket_lwt_unix.close_transport conn >>= fun _ -> Lwt.return None
  in
  let pushf () =
    send ()
    |> Lwt_stream.map_s (function
      | Some `Leave ->
        Lwt_log.debug ~section "Got EOF. Sending a close frame."
        >>= fun () ->
        write conn (Frame.create ~opcode:Close ())
        >>= fun () ->
        close_sent := true;
        Lwt.return_unit
      | Some (`Move _ as move) ->
        write
          conn
          (Frame.create ~content:(Message.Serializer.string_of_client_message move) ())
        >>= fun _ -> Lwt.return_unit
      | _ -> Lwt.return_unit)
  in
  [ pushf (); Lwt_stream.from react ]
  |> Lwt_stream.choose
  |> Lwt_stream.iter_s (fun _ -> Lwt.return ())
;;
