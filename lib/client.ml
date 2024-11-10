open Notty
open Notty_lwt

let hidden_background i : image =
  I.uchar A.(fg lightblack ++ bg black) (Uchar.of_int (0x2591 + i)) 1 1
;;

let fog_gradient visibility_ratio : image =
  let scale = 23.0 *. Base.Float.clamp_exn visibility_ratio ~min:0.0 ~max:1.0 in
  I.uchar A.(bg (gray @@ int_of_float scale)) (Uchar.of_int 0x2591) 1 1
;;

let visible_background : image = I.uchar A.(bg black) (Uchar.of_char ' ') 1 1

module Map = Map.Make (struct
    type t = int * int

    let compare = Stdlib.compare
  end)

let fog_env distance_sq view_radius_sq =
  let view_radius_sq = float_of_int view_radius_sq in
  if distance_sq > view_radius_sq
  then (
    let distance = sqrt distance_sq in
    let view_radius = view_radius_sq |> sqrt in
    fog_gradient @@ ((distance -. view_radius) /. 20.0))
  else if distance_sq = view_radius_sq
  then hidden_background 0
  else visible_background
;;

let render_entity entity_type distance_sq view_radius_sq =
  let visibility_ratio =
    1.0 -. (float_of_int distance_sq /. float_of_int view_radius_sq)
  in
  let scale =
    5.0 *. Base.Float.clamp_exn (visibility_ratio *. visibility_ratio) ~min:0.0 ~max:1.0
  in
  let color =
    match entity_type with
    | `Player `Human -> A.(rgb ~r:(int_of_float scale) ~g:0 ~b:0)
    | `Player `Zombie -> A.(rgb ~r:0 ~g:(int_of_float scale) ~b:0)
  in
  I.uchar A.(bg color) (Uchar.of_char ' ') 1 1
;;

let dist_sq (ax, ay) (bx, by) =
  let dx, dy = (bx - ax, by - ay) in
  (dx * dx) + (dy * dy)
;;

let is_visible distance_sq view_radius_sq = distance_sq <= view_radius_sq

let render ~me terminal Game.{ config; entities; _ } =
  let entities_set =
    Map.of_list (List.map (fun entity -> Game.((entity.x, entity.y), entity)) entities)
  in
  let Game.{ x = mx; y = my; _ } = List.find (fun e -> e.Game.id = me) entities in
  let image =
    I.tabulate (config.width * 2) config.height
    @@ fun x y ->
    let x = x / 2 in
    let distance_from_player = dist_sq (x, y) (mx, my) in
    (* TODO: use ally/enemy view_radius logic *)
    let view_radius_sq = config.human_view_radius * config.human_view_radius in
    match Map.find_opt (x, y) entities_set with
    | Some { entity_type; _ } when is_visible distance_from_player view_radius_sq ->
      render_entity entity_type distance_from_player view_radius_sq
    | _ -> fog_env (float_of_int distance_from_player) view_radius_sq
  in
  Term.image terminal image
;;

let send_player_input terminal () =
  Lwt_stream.map_s
    (function
      | `Key (`Arrow move, []) -> Lwt.return @@ Some (`Move move)
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
  | `GameOver ->
    exit 0 |> ignore;
    Lwt.return ()
  | `Misc message ->
    print_endline message;
    Lwt.return ()
;;

let join_game game_id =
  let uri = Uri.of_string (Config.server_url ^ "/join/" ^ Int.to_string game_id) in
  let terminal = Term.create () in
  let send_player_input = send_player_input terminal in
  let client_id = ref None in
  let receive = receive client_id terminal in
  Ws_client.client uri receive send_player_input
;;
