open Cmdliner

type command =
  | Create of Game.WireFormat.config
  | Join of int
  | Test of Game.WireFormat.config
  | List

let game_config_term =
  let open Game.WireFormat in
  let mk_config
    width
    height
    human_view_radius
    zombie_view_radius
    max_player_count
    time_limit
    tick_delta
    walls_per_floor
    staircases_per_floor
    number_of_floor
    window_probability
    =
    { width;
      height;
      human_view_radius;
      zombie_view_radius;
      max_player_count;
      time_limit;
      tick_delta;
      theme_name = `Default;
      walls_per_floor;
      staircases_per_floor;
      number_of_floor;
      window_probability
    }
  in
  let opt_int ~short ~long default doc =
    Arg.(value & opt int default & info [ short; long ] ~doc)
  in
  let opt_float ~short ~long default doc =
    Arg.(value & opt float default & info [ short; long ] ~doc)
  in
  let width =
    opt_int ~short:"w" ~long:"width" Config.default_game_config.width "Width of the game"
  in
  let height =
    opt_int
      ~short:"h"
      ~long:"height"
      Config.default_game_config.height
      "Height of the game"
  in
  let human_view_radius =
    opt_int
      ~short:"H"
      ~long:"human-view-radius"
      Config.default_game_config.human_view_radius
      "Human view radius"
  in
  let zombie_view_radius =
    opt_int
      ~short:"Z"
      ~long:"zombie-view-radius"
      Config.default_game_config.zombie_view_radius
      "Zombie view radius"
  in
  let max_player_count =
    opt_int
      ~short:"p"
      ~long:"max-player-count"
      Config.default_game_config.max_player_count
      "Max players"
  in
  let time_limit =
    opt_int
      ~short:"t"
      ~long:"time-limit"
      Config.default_game_config.time_limit
      "Time limit in seconds"
  in
  let tick_delta =
    opt_float
      ~short:"d"
      ~long:"tick-delta"
      Config.default_game_config.tick_delta
      "Tick delta time in seconds"
  in
  let walls_per_floor =
    opt_int
      ~short:"W"
      ~long:"walls-per-floor"
      Config.default_game_config.walls_per_floor
      "Walls per floor"
  in
  let staircases_per_floor =
    opt_int
      ~short:"S"
      ~long:"staircases-per-floor"
      Config.default_game_config.staircases_per_floor
      "Staircases per floor"
  in
  let number_of_floor =
    opt_int
      ~short:"n"
      ~long:"number-of-floor"
      Config.default_game_config.number_of_floor
      "Number of floors"
  in
  let window_probability =
    opt_float
      ~short:"g"
      ~long:"window-probability"
      Config.default_game_config.window_probability
      "Probability of windows appearing in walls (0.0-1.0)"
  in
  Term.(
    const mk_config
    $ width
    $ height
    $ human_view_radius
    $ zombie_view_radius
    $ max_player_count
    $ time_limit
    $ tick_delta
    $ walls_per_floor
    $ staircases_per_floor
    $ number_of_floor
    $ window_probability)
;;

let join_term =
  let game_id =
    Arg.(
      required
      & pos 0 (some int) None
      & info [] ~docv:"GAME_ID" ~doc:"Join a game with the specified ID")
  in
  Term.(const (fun id -> Join id) $ game_id)
;;

let create_term = Term.(const (fun config -> Create config) $ game_config_term)
let test_term = Term.(const (fun config -> Test config) $ game_config_term)
let list_term = Term.(const List)

let join_cmd =
  let info = Cmd.info "join" ~doc:"Join a game by ID" in
  Cmd.v info join_term
;;

let create_cmd =
  let info = Cmd.info "create" ~doc:"Create a new game with custom parameters" in
  Cmd.v info create_term
;;

let test_cmd =
  let info = Cmd.info "test" ~doc:"Run an offline test game with custom parameters" in
  Cmd.v info test_term
;;

let list_cmd =
  let info = Cmd.info "list" ~doc:"List available lobbies waiting for players" in
  Cmd.v info list_term
;;

let main_cmd =
  let info = Cmd.info "client" ~version:"1.0" ~doc:"Multiplayer game client" in
  Cmd.group info [ join_cmd; create_cmd; test_cmd; list_cmd ]
;;

let parse_args () =
  match Cmd.eval_value main_cmd with
  | Ok (`Ok cmd) -> cmd
  | Ok `Version | Ok `Help -> exit 0
  | Error _ -> exit 1
;;
