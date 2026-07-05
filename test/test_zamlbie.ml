(* End-to-end smoke test: starts the real server in-process (on a fixed test
   port), then drives it through the real HTTP and websocket client stacks —
   create a game, list lobbies, join with two players, move around, and play
   until the game ends on the time limit. *)

open Zamlbie

let test_port = 7799

let test_config : Game.WireFormat.config =
  { Config.default_game_config with
    max_player_count = 2;
    time_limit = 3;
    tick_delta = 0.05;
    width = 10;
    height = 10;
    number_of_floor = 1;
    walls_per_floor = 2;
    staircases_per_floor = 0
  }
;;

let check name condition = if not condition then failwith ("check failed: " ^ name)

module Ws_conn = struct
  let receive conn = Message.server_message_of_string (Ws.Raw_client.receive_one conn)
  let send conn msg = Ws.Raw_client.send_one conn (Message.string_of_client_message msg)
end

let expect_joined conn =
  match Ws_conn.receive conn with
  | `Joined id -> id
  | _ -> failwith "expected Joined as first message"
;;

let rec play conn ~moves_left =
  match Ws_conn.receive conn with
  | `Update _ ->
    if moves_left > 0 then Ws_conn.send conn (`Move `Up);
    play conn ~moves_left:(moves_left - 1)
  | `GameOver winner -> winner
  | `Rejected reason -> failwith ("unexpectedly rejected mid-game: " ^ reason)
  | `Misc _ -> play conn ~moves_left
  | `Joined _ -> failwith "duplicate Joined"
;;

let run_test env =
  let server_url = Printf.sprintf "http://127.0.0.1:%d" test_port in
  (* Lobby list starts empty. *)
  (match Network.HttpClient.get ~env (server_url ^ "/lobbies") with
   | Ok (`Lobbies lobbies) -> check "no lobbies initially" (lobbies = [])
   | Ok _ -> failwith "unexpected response to /lobbies"
   | Error e -> failwith (Httpc.Raw_client.show_error e));
  (* Create a game. *)
  let game_id =
    match
      Network.HttpClient.post ~env (server_url ^ "/create_game") (`CreateGame test_config)
    with
    | Ok (`GameCreated game) -> game.Game.WireFormat.game_id
    | Ok _ -> failwith "unexpected response to /create_game"
    | Error e -> failwith (Httpc.Raw_client.show_error e)
  in
  (* It shows up as a waiting lobby. *)
  (match Network.HttpClient.get ~env (server_url ^ "/lobbies") with
   | Ok (`Lobbies [ lobby ]) ->
     check "lobby id" (lobby.Game.WireFormat.game_id = game_id);
     check "lobby players" (lobby.current_players = 0)
   | Ok _ -> failwith "expected exactly one lobby"
   | Error e -> failwith (Httpc.Raw_client.show_error e));
  (* Joining a nonexistent game is rejected. *)
  Eio.Switch.run (fun sw ->
    let bad =
      Ws.Raw_client.connect ~sw ~env (Uri.of_string (server_url ^ "/join/424242"))
    in
    (match Ws_conn.receive bad with
     | `Rejected _ -> ()
     | _ -> failwith "expected Rejected for unknown game");
    Ws.Raw_client.close bad);
  (* A player joining then leaving before the game starts frees the slot. *)
  Eio.Switch.run (fun sw ->
    let early =
      Ws.Raw_client.connect
        ~sw
        ~env
        (Uri.of_string (server_url ^ "/join/" ^ string_of_int game_id))
    in
    let (_ : int) = expect_joined early in
    Ws.Raw_client.close early);
  let rec await_empty_lobby retries =
    if retries = 0 then failwith "lobby never returned to 0 players";
    match Network.HttpClient.get ~env (server_url ^ "/lobbies") with
    | Ok (`Lobbies [ lobby ]) when lobby.Game.WireFormat.current_players = 0 -> ()
    | Ok (`Lobbies [ _ ]) ->
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.1;
      await_empty_lobby (retries - 1)
    | Ok _ -> failwith "expected exactly one lobby after early leave"
    | Error e -> failwith (Httpc.Raw_client.show_error e)
  in
  await_empty_lobby 20;
  (* Run two full games concurrently (they tick on the match-runner domain
     pool); each starts when full and runs to the time limit. *)
  let play_full_game game_id =
    Eio.Switch.run (fun sw ->
      let join () =
        Ws.Raw_client.connect
          ~sw
          ~env
          (Uri.of_string (server_url ^ "/join/" ^ string_of_int game_id))
      in
      let p1 = join () in
      let id1 = expect_joined p1 in
      let p2 = join () in
      let id2 = expect_joined p2 in
      check "distinct player ids" (id1 <> id2);
      let winner1, winner2 =
        Eio.Fiber.pair (fun () -> play p1 ~moves_left:5) (fun () -> play p2 ~moves_left:5)
      in
      check "both players see the same winner" (winner1 = winner2);
      Ws.Raw_client.close p1;
      Ws.Raw_client.close p2;
      winner1)
  in
  let second_game_id =
    match
      Network.HttpClient.post ~env (server_url ^ "/create_game") (`CreateGame test_config)
    with
    | Ok (`GameCreated game) -> game.Game.WireFormat.game_id
    | Ok _ -> failwith "unexpected response to /create_game"
    | Error e -> failwith (Httpc.Raw_client.show_error e)
  in
  let winner_a, winner_b =
    Eio.Fiber.pair
      (fun () -> play_full_game game_id)
      (fun () -> play_full_game second_game_id)
  in
  (match (winner_a, winner_b) with
   | (`Human | `Zombie), (`Human | `Zombie) -> ());
  (* The finished game no longer appears in the lobby list. *)
  (match Network.HttpClient.get ~env (server_url ^ "/lobbies") with
   | Ok (`Lobbies lobbies) -> check "no lobbies after game over" (lobbies = [])
   | Ok _ -> failwith "unexpected response to /lobbies"
   | Error e -> failwith (Httpc.Raw_client.show_error e));
  print_endline "all checks passed"
;;

let () =
  (* The server blocks its domain, so run it in its own and don't join it:
     the test exits the process when the checks are done. *)
  Unix.putenv "PORT" (string_of_int test_port);
  Unix.putenv "ZAMLBIE_SERVER_INTERFACE" "127.0.0.1";
  let (_ : unit Domain.t) = Domain.spawn (fun () -> Server.run ()) in
  Unix.sleepf 0.5;
  Eio_main.run run_test;
  exit 0
;;
