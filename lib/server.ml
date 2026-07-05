open Base
open Eio.Std

let log fmt = Stdlib.Printf.printf (Stdlib.( ^^ ) fmt "\n%!")
let log_error fmt = Stdlib.Printf.eprintf (Stdlib.( ^^ ) fmt "\n%!")

(* Runs match orchestrators on a dedicated pool of domains, so game loops
   never compete with connection handling and many matches can tick in
   parallel. Each worker domain forks a fiber per match; matches spend most
   of their time sleeping between ticks, so a domain can host many. *)
module Match_runner = struct
  type t = (unit -> unit) Eio.Stream.t

  let create ~sw ~domain_mgr domains =
    let jobs : t = Eio.Stream.create 32 in
    for _ = 1 to domains do
      Fiber.fork_daemon ~sw (fun () ->
        Eio.Domain_manager.run domain_mgr (fun () ->
          Switch.run (fun match_sw ->
            let rec worker () =
              let job = Eio.Stream.take jobs in
              Fiber.fork ~sw:match_sw (fun () ->
                try job () with
                | exn -> log_error "match orchestrator crashed: %s" (Exn.to_string exn));
              worker ()
            in
            worker ())))
    done;
    jobs
  ;;

  let spawn t job = Eio.Stream.add t job
end

let game_update_message ?player_id game =
  let entities =
    match player_id with
    | None ->
      (* For global view, convert all server entities to wire entities *)
      game.Game.entities
      |> Hashtbl.data
      |> List.map ~f:(fun ({ entity_type; id; x; y; _ } : Game.entity) ->
        Game.WireFormat.{ entity_type; id; x; y; theme = game.Game.config.theme_name })
    | Some id -> Game.visible_map_relative id game
  in
  `Update (Game.WireFormat.wire_format ~game_id:game.game_id ~entities)
;;

let game_update_message_for_player player_id game =
  let relative_entities = Game.visible_map_relative player_id game in
  `Update (Game.WireFormat.wire_format ~game_id:game.game_id ~entities:relative_entities)
;;

let send_game_updates_for_players game_match =
  Match.players_iter game_match ~f:(fun player_id player ->
    Match.Player.send
      player
      (game_update_message_for_player player_id game_match.Match.state))
;;

(* The orchestrator fiber: exclusive owner of the match state. See match.ml
   for the ownership rules. *)
let match_orchestrator ~clock game_match =
  let handle_join reply =
    match Match.try_join game_match with
    | Error _ as rejected -> Promise.resolve reply rejected
    | Ok ({ player_id; player } : Match.joined) as joined ->
      (* Queue the handshake messages before the session fiber starts
         pumping; stream order guarantees the client sees Joined first. *)
      Match.Player.send player (`Joined player_id);
      Match.Player.send player (game_update_message ~player_id game_match.Match.state);
      Promise.resolve reply joined
  in
  let rec lobby () =
    if not (Match.is_started game_match)
    then (
      match Match.next_event game_match with
      | Join { reply } ->
        handle_join reply;
        lobby ()
      | Disconnect player_id ->
        Match.disconnect game_match player_id;
        lobby ())
  in
  let drain_inbox () =
    let rec drain () =
      match Match.poll_event game_match with
      | None -> ()
      | Some (Join { reply }) ->
        Promise.resolve reply (Error "Game already started!");
        drain ()
      | Some (Disconnect player_id) ->
        Match.disconnect game_match player_id;
        drain ()
    in
    drain ()
  in
  let execute_player_moves () =
    let walls =
      Game.gather_positions
        ~p:(fun e ->
          let open Stdlib in
          e = `Environment `Wall || e = `Environment `Glass)
        ~entities:game_match.Match.state.entities
    in
    Match.players_iter game_match ~f:(fun entity_id player ->
      Match.Player.take_mail player
      |> Stdlib.Option.iter (fun move ->
        Game.move ~walls ~game:game_match.Match.state ~entity_id ~move
        |> Stdlib.Option.iter (Match.update_game_state game_match)))
  in
  let apply_in_game_effects () =
    Effects.(apply Tick.effects game_match.Match.state)
    |> Match.update_game_state game_match
  in
  lobby ();
  send_game_updates_for_players game_match;
  let start_time = Unix.time () in
  let rec tick () =
    Eio.Time.sleep clock game_match.Match.state.config.tick_delta;
    drain_inbox ();
    execute_player_moves ();
    apply_in_game_effects ();
    send_game_updates_for_players game_match;
    match Game.verify_end_conditions game_match.Match.state start_time with
    | None -> tick ()
    | Some (Other _) -> assert false (* future other end state? *)
    | Some (Win who) ->
      Match.Registry.remove game_match.Match.match_id;
      Match.broadcast game_match (`GameOver who)
  in
  tick ()
;;

let sha1 s = Digestif.SHA1.(digest_string s |> to_raw_string)

(* Per-websocket session fiber: joins the match, then pumps the player's
   outbox to the socket while feeding inputs back to the match. *)
let websocket_session ~clock ~wsd ~incoming candidate_match_id =
  (* The peer can close the connection at any moment (its Close frame is
     processed concurrently on the read fiber), so sending on a closing
     websocket is a no-op, not an error. *)
  let send message =
    if not (Httpun_ws.Wsd.is_closed wsd)
    then (
      let payload = Bytes.of_string (Message.string_of_server_message message) in
      try
        Httpun_ws.Wsd.send_bytes
          wsd
          ~kind:`Text
          payload
          ~off:0
          ~len:(Bytes.length payload)
      with
      | Failure _ -> ())
  in
  let close () = if not (Httpun_ws.Wsd.is_closed wsd) then Httpun_ws.Wsd.close wsd in
  let reject reason =
    send (`Rejected reason);
    close ()
  in
  match Match.Registry.find candidate_match_id with
  | None -> reject "Game not found!"
  | Some game_match ->
    let reply, resolve_reply = Promise.create () in
    Match.post game_match (Match.Join { reply = resolve_reply });
    (* The timeout covers the race where the orchestrator finishes (game
       over) between the registry lookup and it seeing our Join event. *)
    let join_result =
      Eio.Time.with_timeout clock 10.0 (fun () ->
        match Promise.await reply with
        | Ok joined -> Ok (Ok joined)
        | Error reason -> Ok (Error reason))
    in
    (match join_result with
     | Error `Timeout -> reject "Game not available!"
     | Ok (Error reason) -> reject reason
     | Ok (Ok ({ player_id; player } : Match.joined)) ->
       let pump_outbox () =
         let rec pump () =
           let message = Match.Player.receive player in
           send message;
           match message with
           | `GameOver _ -> close ()
           | _ -> pump ()
         in
         pump ()
       in
       let handle_inputs () =
         let disconnect () = Match.post game_match (Match.Disconnect player_id) in
         let rec handle () =
           match Eio.Stream.take incoming with
           | `Eof -> disconnect ()
           | `Msg raw ->
             (match Message.client_message_of_string raw with
              | `Move move ->
                Match.Player.mail move player;
                handle ()
              | `Quit ->
                disconnect ();
                close ()
              | exception _ ->
                log_error "ignoring malformed client message: %s" raw;
                handle ())
         in
         handle ()
       in
       (* Whichever side finishes first (game over on the outbox side,
          disconnect on the input side) cancels the other. *)
       Fiber.first pump_outbox handle_inputs)
;;

let read_body reqd continue =
  let body = Httpun.Reqd.request_body reqd in
  let buffer = Stdlib.Buffer.create 1024 in
  let rec read () =
    Httpun.Body.Reader.schedule_read
      body
      ~on_read:(fun bs ~off ~len ->
        Stdlib.Buffer.add_string buffer (Bigstringaf.substring bs ~off ~len);
        read ())
      ~on_eof:(fun () -> continue (Stdlib.Buffer.contents buffer))
  in
  read ()
;;

let respond_string reqd status body =
  let headers =
    Httpun.Headers.of_list
      [ ("content-type", "application/json");
        ("content-length", Int.to_string (String.length body))
      ]
  in
  Httpun.Reqd.respond_with_string reqd (Httpun.Response.create ~headers status) body
;;

let request_handler ~conn_sw ~clock ~runner _client_addr { Gluten.Reqd.reqd; upgrade } =
  let request : Httpun.Request.t = Httpun.Reqd.request reqd in
  match (request.meth, request.target) with
  | `POST, "/create_game" ->
    read_body reqd (fun body ->
      match Message.http_request_of_string body with
      | `CreateGame config ->
        let game_match = Match.Registry.new_match config in
        (* Snapshot the response before the orchestrator takes ownership of
           the state. *)
        let (`Update game) = game_update_message game_match.Match.state in
        Match_runner.spawn runner (fun () -> match_orchestrator ~clock game_match);
        log "created game %d" game_match.Match.match_id;
        respond_string reqd `OK (Message.string_of_http_response (`GameCreated game))
      | `GetLobbies ->
        respond_string
          reqd
          `Bad_request
          (Message.string_of_http_response
             (`HttpError "Invalid request type for /create_game"))
      | exception _ ->
        respond_string
          reqd
          `Bad_request
          (Message.string_of_http_response (`HttpError "Malformed request")))
  | `GET, "/lobbies" ->
    let lobbies = Match.Registry.list_waiting_matches () in
    respond_string reqd `OK (Message.string_of_http_response (`Lobbies lobbies))
  | `GET, target when String.is_prefix target ~prefix:"/join/" ->
    (match Int.of_string_opt (String.drop_prefix target 6) with
     | None ->
       respond_string
         reqd
         `Bad_request
         (Message.string_of_http_response (`HttpError "Invalid game id"))
     | Some match_id ->
       let upgrade_handler () =
         let incoming = Ws.Frames.create_incoming () in
         let ws_connection =
           Httpun_ws.Server_connection.create_websocket (fun wsd ->
             (* The session fiber runs under the connection's switch and in
                the connection's domain; only it touches [wsd]. *)
             Fiber.fork ~sw:conn_sw (fun () ->
               websocket_session ~clock ~wsd ~incoming match_id);
             Ws.Frames.handlers ~wsd incoming)
         in
         upgrade (Gluten.make (module Httpun_ws.Server_connection) ws_connection)
       in
       (match Httpun_ws.Handshake.respond_with_upgrade ~sha1 reqd upgrade_handler with
        | Ok () -> ()
        | Error error ->
          respond_string
            reqd
            `Bad_request
            (Message.string_of_http_response (`HttpError error))))
  | _ ->
    respond_string
      reqd
      `Not_found
      (Message.string_of_http_response (`HttpError "Not found"))
;;

let error_handler _client_addr ?request:_ error start_response =
  let message =
    match error with
    | `Exn exn -> Exn.to_string exn
    | (#Httpun.Status.client_error | #Httpun.Status.server_error) as status ->
      Httpun.Status.to_string status
  in
  let body = start_response Httpun.Headers.empty in
  Httpun.Body.Writer.write_string body message;
  Httpun.Body.Writer.close body
;;

let connection_handler ~clock ~runner client_addr client_socket =
  Switch.run (fun conn_sw ->
    Httpun_eio.Server.create_connection_handler
      ~request_handler:(request_handler ~conn_sw ~clock ~runner)
      ~error_handler
      ~sw:conn_sw
      client_addr
      client_socket)
;;

let run () =
  let port = Config.get_server_port () in
  let interface = Config.get_server_interface () in
  Eio_main.run
  @@ fun env ->
  Switch.run
  @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  (* Split the available cores between game loops and connection handling;
     the main domain always runs an accept loop, extra domains add more. *)
  let total_domains = Stdlib.Domain.recommended_domain_count () in
  let match_domains = Int.max 1 (total_domains / 2) in
  let extra_accept_domains = Int.max 0 (total_domains - 1 - match_domains) in
  log
    "Starting server on %s:%d (%d match domain(s), %d accept domain(s))"
    interface
    port
    match_domains
    (extra_accept_domains + 1);
  let listen_address =
    `Tcp (Eio_unix.Net.Ipaddr.of_unix (Unix.inet_addr_of_string interface), port)
  in
  let listening_socket =
    Eio.Net.listen ~reuse_addr:true ~backlog:64 ~sw net listen_address
  in
  let runner = Match_runner.create ~sw ~domain_mgr match_domains in
  let accept_loop () =
    Switch.run
    @@ fun accept_sw ->
    let rec loop () =
      Eio.Net.accept_fork
        listening_socket
        ~sw:accept_sw
        ~on_error:(fun exn -> log_error "connection error: %s" (Exn.to_string exn))
        (fun client_socket client_addr ->
           connection_handler ~clock ~runner client_addr client_socket);
      loop ()
    in
    loop ()
  in
  for _ = 1 to extra_accept_domains do
    Fiber.fork_daemon ~sw (fun () -> Eio.Domain_manager.run domain_mgr accept_loop)
  done;
  accept_loop ()
;;
