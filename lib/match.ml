open Base

type player_t =
  { websocket : Dream.websocket;
    mutable mailbox : Game.move option
  }

type t =
  { players : (Int.t, player_t) Hashtbl.t;
    started : unit Lwt_condition.t;
    mutable state : Game.game
  }

let update_game_state t new_state = t.state <- new_state
let player_count t = Hashtbl.length t.players

let mailbox_move t player_id move =
  Hashtbl.find t.players player_id
  |> Stdlib.Option.iter (fun player -> player.mailbox <- Some move)
;;

let players_ws_iter t ~f = Hashtbl.iter t.players ~f:(fun { websocket; _ } -> f websocket)

let start t =
  let start_game_state = Game.apply_start_effects t.state in
  update_game_state t start_game_state;
  Lwt_condition.signal t.started ()
;;

let try_join_match t websocket =
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
    Hashtbl.set t.players ~key:player_id ~data:{ websocket; mailbox = None };
    if new_player_count = room_size then start t;
    Some player_id)
;;

module Registry = struct
  open Base

  let matches : (Int.t, t) Hashtbl.t = Hashtbl.create (module Int)
  let find_exn id = Hashtbl.find_exn matches id
  let find id = Hashtbl.find matches id
  let remove id = Hashtbl.remove matches id

  let new_match config thread =
    let match_id = Uuid.next_id () in
    let new_game = Game.make match_id config in
    Hashtbl.add_exn
      matches
      ~key:match_id
      ~data:
        { players = Hashtbl.create (module Int);
          started = Lwt_condition.create ();
          state = new_game
        };
    Lwt_preemptive.detach thread match_id |> ignore;
    new_game
  ;;
end
