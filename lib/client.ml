open Notty
open Eio.Std

module Map = Map.Make (struct
    type t = int * int

    let compare = Stdlib.compare
  end)

let create_animated_fog x y center_x center_y =
  let time = Unix.time () in
  let noise_x = float_of_int (x + center_x) in
  let noise_y = float_of_int (y + center_y) in
  let noise_value =
    (sin ((time *. 0.5) +. (noise_x *. 0.1) +. (noise_y *. 0.1)) *. 0.1)
    +. (sin ((time *. 0.3) +. (noise_x *. 0.05)) *. 0.05)
    +. (cos ((time *. 0.7) +. (noise_y *. 0.08)) *. 0.03)
  in
  let fog_alpha = 0.8 +. noise_value in
  (`Fog, Base.Float.clamp_exn fog_alpha ~min:0.6 ~max:1.0, `Default)
;;

let render_relative terminal Game.WireFormat.{ entities; _ } =
  let window_height, window_width = (21, 21) in
  let center_x, center_y = (window_width / 2, window_height / 2) in
  let entities_set =
    Map.of_list
      (List.map
         (fun entity -> ((entity.Game.WireFormat.x, entity.Game.WireFormat.y), entity))
         entities)
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
  Notty_eio.Term.image terminal image
;;

let create_game ~env ~server_url config =
  let url = server_url ^ "/create_game" in
  (* Use 60 second timeout for create_game to handle cold startup *)
  match Network.HttpClient.post ~env ~timeout:60.0 url (`CreateGame config) with
  | Ok (`GameCreated game) -> Ok game
  | Ok (`HttpError msg) -> Error (`ClientError (400, msg))
  | Ok _ -> Error (`UnexpectedError (500, "Invalid response type"))
  | Error err -> Error err
;;

let list_lobbies ~env ~server_url () =
  let url = server_url ^ "/lobbies" in
  (* Use default 10 second timeout for list_lobbies *)
  match Network.HttpClient.get ~env ~timeout:10.0 url with
  | Ok (`Lobbies lobbies) ->
    (match lobbies with
     | [] -> print_endline "No lobbies available."
     | _ ->
       print_endline "Available Lobbies:";
       List.iter
         (fun (lobby : Game.WireFormat.lobby_info) ->
            Printf.printf
              "Game %d: %d/%d players (%s)\n"
              lobby.game_id
              lobby.current_players
              lobby.max_players
              lobby.config_preview)
         lobbies;
       print_endline "\nUse: dune exec client -- join <game_id>")
  | Ok (`HttpError msg) -> Printf.printf "Server error: %s\n" msg
  | Ok _ -> Printf.printf "Unexpected response type\n"
  | Error err ->
    Printf.printf "Error fetching lobbies: %s\n" (Httpc.Raw_client.show_error err)
;;

(* Runs the TUI: one fiber renders whatever the server sends, the other
   forwards key presses. Whichever finishes first (game over / disconnect /
   escape) wins and tears the terminal down cleanly via the switch. *)
let run_game_ui ~env conn =
  let events = Eio.Stream.create 64 in
  let outcome =
    Notty_eio.Term.run
      ~input:(Eio.Stdenv.stdin env)
      ~output:(Eio.Stdenv.stdout env)
      ~on_event:(fun event -> Eio.Stream.add events event)
      (fun terminal ->
         let rec server_loop () =
           match Network.WsClient.receive_one conn with
           | `Update updated_game ->
             render_relative terminal updated_game;
             server_loop ()
           | `GameOver character -> `Winner character
           | `Rejected reason -> `Error reason
           | `Misc _ -> server_loop ()
           | `Joined _ -> `Error "'Joined' should only be received once"
           | exception End_of_file -> `Disconnected
         in
         let rec input_loop () =
           match Eio.Stream.take events with
           | `Key (`Arrow move, []) ->
             Network.WsClient.send_one conn (`Move move);
             input_loop ()
           | `Key (`Escape, []) ->
             Network.WsClient.close conn;
             `Quit
           | _ -> input_loop ()
         in
         Fiber.first server_loop input_loop)
  in
  (* The terminal is restored here; safe to print. *)
  match outcome with
  | `Winner character ->
    print_endline (Message.string_of_character_type character ^ " won!")
  | `Quit -> ()
  | `Disconnected -> print_endline "Disconnected from server."
  | `Error reason -> failwith reason
;;

let join_game ~env ~sw ~server_url game_id =
  let uri = Uri.of_string (server_url ^ "/join/" ^ Int.to_string game_id) in
  let conn = Network.WsClient.connect ~sw ~env uri in
  match Network.WsClient.receive_one conn with
  | `Joined _assigned_client_id -> run_game_ui ~env conn
  | `Rejected reason -> print_endline ("Joining game failed: " ^ reason)
  | _ -> failwith "First websocket message from server should be 'Joined' or 'Rejected'"
;;

let offline_game ~env config =
  let game_update_message player_id game =
    let entities = Game.visible_map_relative player_id game in
    Game.WireFormat.wire_format ~game_id:game.game_id ~entities
  in
  let initialize_game config =
    let base_game = Game.make 0 config in
    let game_with_player = Game.add_entity base_game Game.default_entity |> snd in
    ref @@ Effects.(apply Start.effects game_with_player)
  in
  let game = initialize_game config in
  let events = Eio.Stream.create 64 in
  Notty_eio.Term.run
    ~input:(Eio.Stdenv.stdin env)
    ~output:(Eio.Stdenv.stdout env)
    ~on_event:(fun event -> Eio.Stream.add events event)
    (fun terminal ->
       render_relative terminal (game_update_message 0 !game);
       let handle_move move =
         let walls =
           Game.gather_positions
             ~p:(fun e -> e = `Environment `Wall || e = `Environment `Glass)
             ~entities:!game.Game.entities
         in
         Game.move ~walls ~game:!game ~entity_id:0 ~move
         |> Option.iter (fun ngame -> game := ngame);
         (game := Effects.(apply Tick.effects !game));
         render_relative terminal (game_update_message 0 !game)
       in
       let rec loop () =
         match Eio.Stream.take events with
         | `Key (`Arrow move, []) ->
           handle_move move;
           loop ()
         | `Key (`Escape, []) -> ()
         | _ -> loop ()
       in
       loop ())
;;
