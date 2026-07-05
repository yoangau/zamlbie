(* Bridge between httpun-ws's callback-style frame input and a direct-style
   [Eio.Stream.t] of whole (possibly fragmented) websocket messages. Used by
   both the server's connection handler and the client. *)

type incoming =
  [ `Msg of string
  | `Eof
  ]

(* Capacity of the incoming message stream. The frame callbacks below run on
   the connection's read fiber; a large capacity means [Eio.Stream.add] never
   blocks in practice for our tiny JSON messages, while still bounding memory
   against a flooding peer (once full, the read loop is back-pressured). *)
let incoming_capacity = 512
let create_incoming () : incoming Eio.Stream.t = Eio.Stream.create incoming_capacity

(* [handlers ~wsd incoming] returns the [input_handlers] callbacks that
   reassemble text/binary frames into whole messages pushed onto [incoming].
   Sends [`Eof] exactly once, when the peer closes or the transport dies.

   Subtlety: payload bytes are delivered through [Payload.schedule_read]
   callbacks that can complete *after* the close/eof notification for the
   connection (e.g. the final message of a connection the peer closes right
   after sending it). Pushing [`Eof] eagerly would make it overtake that
   final message, so while a payload is in flight the eof is only recorded
   and pushed once the payload completes. *)
let handlers ~wsd incoming =
  let message_buffer = Buffer.create 256 in
  let eof_sent = ref false in
  let payload_in_flight = ref false in
  let eof_pending = ref false in
  let send_eof () =
    if not !eof_sent
    then (
      eof_sent := true;
      Eio.Stream.add incoming `Eof)
  in
  let notice_eof () = if !payload_in_flight then eof_pending := true else send_eof () in
  let frame ~(opcode : Httpun_ws.Websocket.Opcode.t) ~is_fin ~len:_ payload =
    match opcode with
    | `Text | `Binary | `Continuation ->
      payload_in_flight := true;
      let rec drain () =
        Httpun_ws.Payload.schedule_read
          payload
          ~on_read:(fun bs ~off ~len ->
            Buffer.add_string message_buffer (Bigstringaf.substring bs ~off ~len);
            drain ())
          ~on_eof:(fun () ->
            if is_fin
            then (
              let message = Buffer.contents message_buffer in
              Buffer.clear message_buffer;
              Eio.Stream.add incoming (`Msg message));
            payload_in_flight := false;
            if !eof_pending then send_eof ())
      in
      drain ()
    | `Connection_close ->
      if not (Httpun_ws.Wsd.is_closed wsd) then Httpun_ws.Wsd.close wsd;
      notice_eof ()
    | `Ping -> Httpun_ws.Wsd.send_pong wsd
    | `Pong | `Other _ -> ()
  in
  let eof ?error:_ () = notice_eof () in
  { Httpun_ws.Websocket_connection.frame; eof }
;;
