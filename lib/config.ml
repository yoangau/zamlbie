type t = Game.WireFormat.config

let server_url = "http://127.0.0.1:7777"

let default_game_config : Game.WireFormat.config =
  { human_view_radius = 8;
    zombie_view_radius = 6;
    width = 20;
    height = 20;
    max_player_count = 2;
    time_limit = 120;
    tick_delta = 0.5;
    theme_name = `Default;
    walls_per_floor = 10;
    staircases_per_floor = 2;
    number_of_floor = 3;
    window_probability = 0.3
  }
;;
