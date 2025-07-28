open Notty
open Notty_lwt

module Map = Map.Make (struct
    type t = int * int

    let compare = Stdlib.compare
  end)

let create_animated_fog x y center_x center_y =
  let time = Unix.time () in
  let noise_x = float_of_int (x + center_x) in
  let noise_y = float_of_int (y + center_y) in
  let noise_value = 
    sin (time *. 0.5 +. noise_x *. 0.1 +. noise_y *. 0.1) *. 0.1 +.
    sin (time *. 0.3 +. noise_x *. 0.05) *. 0.05 +.
    cos (time *. 0.7 +. noise_y *. 0.08) *. 0.03
  in
  let fog_alpha = 0.8 +. noise_value in
  (`Fog, Base.Float.clamp_exn fog_alpha ~min:0.6 ~max:1.0, `Default)
;;


let render_relative terminal Game.WireFormat.{ entities; _ } =
  let window_height, window_width = (21, 21) in
  let center_x, center_y = (window_width / 2, window_height / 2) in
  let entities_set =
    Map.of_list
      (List.map (fun entity -> ((entity.Game.WireFormat.x, entity.Game.WireFormat.y), entity)) entities)
  in
  let image =
    I.tabulate (window_width * 2) window_height
    @@ fun wx wy ->
    let wx = wx / 2 in
    let x = wx - center_x in
    let y = wy - center_y in
    let tile, alpha, theme_name =
      match Map.find_opt (x, y) entities_set with
      | Some { entity_type; theme; _ } ->
        (* Server sent an entity for this position - render with distance-based alpha *)
        let tile = 
          match entity_type with
          | `Player player_type -> (player_type :> World.tile)
          | `Environment env_type -> (env_type :> World.tile)
        in
        (tile, 1.0, theme)
      | None -> 
        (* No entity sent by server for this position - render animated fog *)
        create_animated_fog x y center_x center_y
    in
    World.render_tile theme_name tile ~alpha
  in
  Term.image terminal image
;;

let send_player_input terminal () =
  Lwt_stream.map_s
    (function
      | `Key (`Arrow move, []) -> Lwt.return @@ Some (`Message (`Move move))
      | `Key (`Escape, []) ->
        let%lwt () = Term.release terminal in
        Lwt.return @@ Some `Close
      | _ -> Lwt.return @@ None)
    (Term.events terminal)
;;

let receive terminal message =
  match message with
  | `Update updated_game -> render_relative terminal updated_game
  | `Rejected reason ->
    failwith reason |> ignore;
    Lwt.return ()
  | `GameOver character ->
    let%lwt () = Term.release terminal in
    print_endline @@ Message.string_of_character_type character ^ " won!";
    exit 0
  | `Misc message ->
    print_endline message;
    Lwt.return ()
  | `Joined _ -> failwith "'Joined' should only be received once"
;;

let create_game config =
  let open Lwt.Infix in
  let open Game.WireFormat in
  let url = Config.server_url ^ "/create_game" in
  Http.Raw_client.post url (Serializer.string_of_config config)
  >>= fun s -> Result.map Serializer.game_of_string s |> Lwt.return
;;

let list_lobbies () =
  let open Lwt.Infix in
  let open Game.WireFormat in
  let url = Config.server_url ^ "/lobbies" in
  Http.Raw_client.get url
  >>= function
  | Ok body -> 
    let lobbies = Serializer.lobby_list_of_string body in
    (match lobbies with
     | [] -> print_endline "No lobbies available."
     | _ -> 
       print_endline "Available Lobbies:";
       List.iter (fun (lobby : Game.WireFormat.lobby_info) ->
         Printf.printf "Game %d: %d/%d players (%s)\n" 
           lobby.game_id 
           lobby.current_players 
           lobby.max_players 
           lobby.config_preview
       ) lobbies;
       print_endline "\nUse: dune exec client -- join <game_id>");
    Lwt.return_unit
  | Error err -> 
    Printf.printf "Error fetching lobbies: %s\n" (Http.Raw_client.show_error err);
    Lwt.return_unit
;;

let join_game terminal game_id =
  let open Network in
  let uri = Uri.of_string (Config.server_url ^ "/join/" ^ Int.to_string game_id) in
  let%lwt conn = WsClient.connect uri in
  let%lwt mandated_join_message = WsClient.receive_one conn in
  (match mandated_join_message with
   | `Joined _assigned_client_id -> ()
   | `Rejected reason ->
     print_endline ("Joining game failed: " ^ reason);
     exit 0
   | _ -> failwith "First websocket message from server should be 'Joined' or 'Rejected'");
  let send_player_input = send_player_input terminal in
  let receive = receive terminal in
  WsClient.duplex conn receive send_player_input
;;

let offline_game terminal config =
  let game_update_message player_id game =
    let entities = Game.visible_map_relative player_id game in
    Game.WireFormat.wire_format ~game_id:game.game_id ~entities
  in
  let initialize_game config =
    let base_game = Game.make 0 config in
    let game_with_player = Game.add_entity base_game Game.default_entity |> snd in
    ref @@ Effects.(apply Start.effects game_with_player)
  in
  let handle_input game = function
    | Some (`Move move) ->
      let walls =
        Game.gather_positions
          ~p:(fun e -> e = `Environment `Wall || e = `Environment `Glass)
          ~entities:!game.Game.entities
      in
      Game.move ~walls ~game:!game ~entity_id:0 ~move
      |> Option.iter (fun ngame -> game := ngame);
      (game := Effects.(apply Tick.effects !game));
      render_relative terminal (game_update_message 0 !game)
    | Some `Close -> exit 1
    | _ -> Lwt.return_unit
  in
  let game = initialize_game config in
  Lwt_stream.map_s
    (function
      | `Key (`Arrow move, []) -> Lwt.return @@ Some (`Move move)
      | `Key (`Escape, []) -> Lwt.return @@ Some `Close
      | _ -> Lwt.return @@ None)
    (Term.events terminal)
  |> Lwt_stream.iter_s (handle_input game)
;;
