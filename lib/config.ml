type t = Game.WireFormat.config

let default_server_url = "http://127.0.0.1:7777"
let default_server_port = 7777
let default_server_interface = "0.0.0.0"

(* Get server URL from provided value, environment variable, or default *)
let get_server_url ?(url : string option) () : string =
  match url with
  | Some u -> u
  | None ->
    (try Sys.getenv "ZAMLBIE_SERVER_URL"
     with Not_found -> default_server_url)
;;

(* Get server port from environment variable or default *)
let get_server_port () : int =
  (* Check PORT first (used by Render.com and other platforms), then ZAMLBIE_SERVER_PORT *)
  try Sys.getenv "PORT" |> int_of_string
  with Not_found ->
    (try Sys.getenv "ZAMLBIE_SERVER_PORT" |> int_of_string
     with Not_found -> default_server_port)
;;

(* Get server interface from environment variable or default *)
let get_server_interface () : string =
  try Sys.getenv "ZAMLBIE_SERVER_INTERFACE"
  with Not_found -> default_server_interface
;;

let default_game_config : Game.WireFormat.config =
  { human_view_radius = 10;
    zombie_view_radius = 8;
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
