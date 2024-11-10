let server_url = "http://127.0.0.1:7777"

let default_game_config : Game.config =
  { human_view_radius = 8;
    zombie_view_radius = 4;
    width = 20;
    height = 20;
    max_player_count = 2;
    time_limit = 30;
    tick_delta = 0.5
  }
;;
