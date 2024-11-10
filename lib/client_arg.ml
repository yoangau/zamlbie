type command =
  | Create of Game.config
  | Join of int

let parse_args () =
  let game_id = ref None in
  let width = ref Config.default_game_config.width in
  let height = ref Config.default_game_config.height in
  let human_view_radius = ref Config.default_game_config.human_view_radius in
  let zombie_view_radius = ref Config.default_game_config.zombie_view_radius in
  let max_player_count = ref Config.default_game_config.max_player_count in
  let time_limit = ref Config.default_game_config.time_limit in
  let tick_delta = ref Config.default_game_config.tick_delta in
  let mode = ref None in
  let specs =
    [ ( "--join",
        Arg.Int
          (fun id ->
            mode := Some "join";
            game_id := Some id),
        "Join a game with specified ID" );
      ("--create", Arg.Unit (fun () -> mode := Some "create"), "Create a new game");
      ("--width", Arg.Set_int width, "Width of the game (default 20)");
      ("--height", Arg.Set_int height, "Height of the game (default 20)");
      ( "--human-view-radius",
        Arg.Set_int human_view_radius,
        "Human view radius (default 5)" );
      ( "--zombie-view-radius",
        Arg.Set_int zombie_view_radius,
        "Zombie view radius (default 5)" );
      ( "--max-player-count",
        Arg.Set_int max_player_count,
        "Maximum number of players (default 2)" );
      ("--time-limit", Arg.Set_int time_limit, "Time limit in seconds (default 60)");
      ( "--tick-delta",
        Arg.Set_float tick_delta,
        "Tick delta time in seconds (default 0.5)" )
    ]
  in
  let usage_msg = "Usage: client [--join <int> | --create [options]]" in
  Arg.parse specs (fun _ -> ()) usage_msg;
  match !mode with
  | Some "join" ->
    (match !game_id with
     | Some id -> Ok (Join id)
     | None ->
       Printf.eprintf "Error: --join requires an integer game ID.\n";
       Error ())
  | Some "create" ->
    Ok
      (Create
         Game.
           { width = !width;
             height = !height;
             human_view_radius = !human_view_radius;
             zombie_view_radius = !zombie_view_radius;
             max_player_count = !max_player_count;
             time_limit = !time_limit;
             tick_delta = !tick_delta
           })
  | _ ->
    Printf.eprintf "Error: specify either --join <int> or --create with options.\n";
    Arg.usage specs usage_msg;
    Error ()
;;
