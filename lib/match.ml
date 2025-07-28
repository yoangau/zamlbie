open Base

module Player = struct
  type t =
    { websocket : Dream.websocket;
      mutable mailbox : Game.WireFormat.move option
    }

  let make ~ws = { websocket = ws; mailbox = None }
  let mail move player = player.mailbox <- Some move

  let take_mail player =
    let old = player.mailbox in
    player.mailbox <- None;
    old
  ;;
end

type t =
  { players : (Uuid.HashtblKey.t, Player.t) Hashtbl.t;
    started : unit Lwt_condition.t;
    mutable state : Game.t
  }

let update_game_state t new_state = t.state <- new_state
let player_count t = Hashtbl.length t.players

let mailbox_move t player_id move =
  Hashtbl.find t.players player_id |> Stdlib.Option.iter (Player.mail move)
;;

let players_ws_iter t ~f =
  Hashtbl.iteri t.players ~f:(fun ~key ~data:{ websocket; _ } -> f websocket key)
;;

let start t =
  let start_game_state = Effects.(apply Start.effects t.state) in
  update_game_state t start_game_state;
  Lwt_condition.signal t.started ()
;;

let try_join_match t ws =
  let room_size = t.state.config.max_player_count in
  let previous_player_count = player_count t in
  let new_player_count = previous_player_count + 1 in
  if previous_player_count >= room_size
  then None
  else (
    let player_id, game =
      Game.add_entity t.state { Game.default_entity with entity_type = `Player `Human }
    in
    t.state <- game;
    Hashtbl.set t.players ~key:player_id ~data:(Player.make ~ws);
    if new_player_count = room_size then start t;
    Some player_id)
;;

let disconnect t player_id =
  let updated_game = Game.remove_entity t.state player_id in
  t.state <- updated_game;
  Hashtbl.remove t.players player_id
;;

module Registry = struct
  open Base

  let matches = Hashtbl.create (module Uuid.HashtblKey)
  let find_exn id = Hashtbl.find_exn matches id
  let find id = Hashtbl.find matches id
  let remove id = Hashtbl.remove matches id
  let next_id_gen = Uuid.create_gen ()

  let new_match config thread =
    let match_id = Uuid.next_id next_id_gen in
    let new_game = Game.make match_id config in
    Hashtbl.add_exn
      matches
      ~key:match_id
      ~data:
        { players = Hashtbl.create (module Uuid.HashtblKey);
          started = Lwt_condition.create ();
          state = new_game
        };
    Lwt.dont_wait (fun () -> thread match_id) (fun _ -> ());
    new_game
  ;;

  let list_waiting_matches () =
    Hashtbl.to_alist matches
    |> List.filter_map ~f:(fun (match_id, match_data) ->
      if Lwt.is_sleeping (Lwt_condition.wait match_data.started)
      then (
        let current_players = player_count match_data in
        let max_players = match_data.state.config.max_player_count in
        let config_preview =
          Printf.sprintf
            "%dx%d, %d floors"
            match_data.state.config.width
            match_data.state.config.height
            match_data.state.config.number_of_floor
        in
        Some
          Game.WireFormat.
            { game_id = match_id; current_players; max_players; config_preview })
      else None)
  ;;
end
