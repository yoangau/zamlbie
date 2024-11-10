type t =
  { players : (Game.move option * Dream.websocket option) array;
    mutable started : bool;
    mutable state : Game.game
  }

let update_game_state t new_state = t.state <- new_state

let player_count t =
  Stdlib.Array.fold_left
    (fun acc (_, s) -> acc + if Option.is_some s then 1 else 0)
    0
    t.players
;;

let mailbox_move t player_id move =
  t.players.(player_id) <- (Some move, snd t.players.(player_id))
;;

let try_join_match t player_socket =
  let room_size = t.state.config.max_player_count in
  let previous_player_count = player_count t in
  let new_player_count = previous_player_count + 1 in
  let player_id = previous_player_count in
  if previous_player_count = room_size
  then None
  else (
    if previous_player_count < room_size
    then (
      t.players.(player_id) <- (None, Some player_socket);
      t.state <- Game.add_entity t.state { Game.default_entity with id = player_id });
    if new_player_count = room_size then t.started <- true;
    Some player_id)
;;

module Registry = struct
  open Base

  let matches : (Int.t, t) Hashtbl.t = Hashtbl.create (module Int)
  let find_exn id = Hashtbl.find_exn matches id
  let find id = Hashtbl.find matches id
  let next_id_gen = ref 0

  let next_id () =
    let id = !next_id_gen in
    Int.incr next_id_gen;
    id
  ;;

  let new_match config thread =
    let match_id = next_id () in
    let new_game = Game.make match_id config in
    Hashtbl.add_exn
      matches
      ~key:match_id
      ~data:
        { players = Stdlib.Array.make new_game.config.max_player_count (None, None);
          started = true;
          state = new_game
        };
    Lwt_preemptive.detach thread match_id |> ignore;
    new_game
  ;;
end
