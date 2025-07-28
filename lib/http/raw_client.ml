type error =
  [ `ClientError of int * string
  | `ServerError of int * string
  | `UnexpectedError of int * string
  ]

let show_error = function
  | `ClientError (code, message) -> Format.sprintf "Client error (%d): %s\n" code message
  | `ServerError (code, message) -> Format.sprintf "Server error (%d): %s\n" code message
  | `UnexpectedError (code, message) ->
    Format.sprintf "Unexpected response (%d): %s\n" code message
;;

let post url body =
  let open Lwt.Infix in
  let open Cohttp in
  let headers = Header.init_with "Content-Type" "application/json" in
  let body = Cohttp_lwt.Body.of_string body in
  let handle_response (resp, body) =
    let status_code = Response.status resp |> Code.code_of_status in
    Cohttp_lwt.Body.to_string body
    >|= fun body_string ->
    match status_code with
    | 200 | 201 -> Ok body_string
    | code when code >= 400 && code < 500 -> Error (`ClientError (code, body_string))
    | code when code >= 500 -> Error (`ServerError (code, body_string))
    | _ -> Error (`UnexpectedError (status_code, body_string))
  in
  Cohttp_lwt_unix.Client.post ~headers ~body (Uri.of_string url) >>= handle_response
;;

let get url =
  let open Lwt.Infix in
  let open Cohttp in
  let handle_response (resp, body) =
    let status_code = Response.status resp |> Code.code_of_status in
    Cohttp_lwt.Body.to_string body
    >|= fun body_string ->
    match status_code with
    | 200 | 201 -> Ok body_string
    | code when code >= 400 && code < 500 -> Error (`ClientError (code, body_string))
    | code when code >= 500 -> Error (`ServerError (code, body_string))
    | _ -> Error (`UnexpectedError (status_code, body_string))
  in
  Cohttp_lwt_unix.Client.get (Uri.of_string url) >>= handle_response
;;
