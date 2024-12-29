open Notty
open Notty_lwt

module Map = Map.Make (struct
    type t = int * int

    let compare = Stdlib.compare
  end)

let fog_env (theme_name : Theme_t.theme_name) distance_sq view_radius_sq =
  let view_radius_sq = float_of_int view_radius_sq in
  (* TODO: This will need to be updated... the theme update broke the fog *)
  if distance_sq > view_radius_sq
  then (
    let distance = sqrt distance_sq in
    let view_radius = view_radius_sq |> sqrt in
    World.render_tile
      theme_name
      (`Environment `Fog)
      ~alpha:(1.0 -. ((distance -. view_radius) /. 20.0)))
  else if distance_sq = view_radius_sq
  then World.render_tile theme_name (`Environment (`Hidden 0))
  else World.render_tile theme_name (`Environment `Visible)
;;

let render_entity (theme_name : Theme_t.theme_name) entity_type distance_sq view_radius_sq
  =
  let visibility_ratio =
    1.0 -. (float_of_int distance_sq /. float_of_int view_radius_sq)
  in
  let scale =
    5.0 *. Base.Float.clamp_exn (visibility_ratio *. visibility_ratio) ~min:0.0 ~max:1.0
  in
  World.render_tile theme_name entity_type ~alpha:scale
;;

let dist_sq (ax, ay) (bx, by) =
  let dx, dy = (bx - ax, by - ay) in
  (dx * dx) + (dy * dy)
;;

let is_visible distance_sq view_radius_sq = distance_sq <= view_radius_sq

let is_outside (x, y) config =
  x < 0 || x >= config.Game.WireFormat.width || y < 0 || y >= config.height
;;

let render ~me terminal Game.WireFormat.{ config; entities; _ } =
  let window_height, window_width = (21, 21) in
  let entities_set =
    Map.of_list
      (List.map (fun entity -> Game.WireFormat.((entity.x, entity.y), entity)) entities)
  in
  let Game.WireFormat.{ x = mx; y = my; entity_type; _ } =
    List.find (fun e -> e.Game.WireFormat.id = me) entities
  in
  let view_radius_sq =
    let vr =
      match entity_type with
      | `Player `Human -> config.human_view_radius
      | `Player `Zombie -> config.zombie_view_radius
      | _ -> failwith "Player should be Player type"
    in
    vr * vr
  in
  let image =
    I.tabulate (window_width * 2) window_height
    @@ fun wx wy ->
    let wx = wx / 2 in
    let gx = mx + wx - (window_width / 2) in
    let gy = my + wy - (window_height / 2) in
    let global_position = (gx, gy) in
    let distance_from_player = dist_sq global_position (mx, my) in
    match Map.find_opt (gx, gy) entities_set with
    | Some { entity_type; _ } when is_visible distance_from_player view_radius_sq ->
      render_entity config.theme_name entity_type distance_from_player view_radius_sq
    | _
      when is_outside global_position config
           && is_visible distance_from_player view_radius_sq ->
      World.render_tile config.theme_name (`Environment `Wall)
    | _ -> fog_env config.theme_name (float_of_int distance_from_player) view_radius_sq
  in
  Term.image terminal image
;;

let send_player_input terminal () =
  Lwt_stream.map_s
    (function
      | `Key (`Arrow move, []) ->
        Lwt.return
        @@ Some (`Message (Message.Serializer.string_of_client_message @@ `Move move))
      | `Key (`Escape, []) -> Lwt.return @@ Some `Leave
      | _ -> Lwt.return @@ None)
    (Term.events terminal)
;;

let receive client_id terminal message =
  match Message.Serializer.server_message_of_string message with
  | `Joined assigned_client_id ->
    client_id := Some assigned_client_id;
    Lwt.return ()
  | `Update updated_game -> render ~me:(Option.get !client_id) terminal updated_game
  | `Rejected reason ->
    failwith reason |> ignore;
    Lwt.return ()
  | `GameOver character ->
    let%lwt () = Term.release terminal in
    print_endline @@ Message.Serializer.string_of_character_type character ^ " won!";
    exit 0
  | `Misc message ->
    print_endline message;
    Lwt.return ()
;;

let create_game config =
  let open Lwt.Infix in
  let open Game.WireFormat in
  let url = Config.server_url ^ "/create_game" in
  Rest_client.post url (Serializer.string_of_config config)
  >>= fun s -> Option.map Serializer.game_of_string s |> Lwt.return
;;

let join_game terminal game_id =
  let uri = Uri.of_string (Config.server_url ^ "/join/" ^ Int.to_string game_id) in
  let send_player_input = send_player_input terminal in
  let client_id = ref None in
  let receive = receive client_id terminal in
  Ws_client.client uri receive send_player_input
;;
