let create_game config =
  let open Lwt.Infix in
  let open Cohttp in
  let headers = Header.init_with "Content-Type" "application/json" in
  let body = Cohttp_lwt.Body.of_string (Game.Serializer.string_of_config config) in
  let url = Config.server_url ^ "/create_game" in
  Cohttp_lwt_unix.Client.post ~headers ~body (Uri.of_string url)
  >>= fun (resp, body) ->
  let status = resp |> Response.status |> Code.code_of_status in
  body
  |> Cohttp_lwt.Body.to_string
  >|= fun body ->
  match status with
  | 200 | 201 -> Some (Game.Serializer.game_of_string body)
  | code when code >= 400 && code < 500 ->
    Printf.printf "Client error (%d): %s\n" code body;
    None
  | code when code >= 500 ->
    Printf.printf "Server error (%d): %s\n" code body;
    None
  | _ ->
    Printf.printf "Unexpected response (%d): %s\n" status body;
    None
;;
