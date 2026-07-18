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

let make_client env =
  let https =
    match Net_support.tls_client_config () with
    | Error _ -> None
    | Ok config ->
      Some
        (fun uri raw ->
          let host = Option.bind (Uri.host uri) Net_support.host_of_string in
          Tls_eio.client_of_flow config ?host raw)
  in
  Cohttp_eio.Client.make ~https (Eio.Stdenv.net env)
;;

let max_response_size = 16 * 1024 * 1024

let classify_response status body_string =
  match status with
  | 200 | 201 -> Ok body_string
  | code when code >= 400 && code < 500 -> Error (`ClientError (code, body_string))
  | code when code >= 500 -> Error (`ServerError (code, body_string))
  | code -> Error (`UnexpectedError (code, body_string))
;;

let request ~env ~timeout url perform =
  let clock = Eio.Stdenv.clock env in
  let result =
    Eio.Time.with_timeout clock timeout (fun () ->
      try
        Eio.Switch.run (fun sw ->
          let client = make_client env in
          let response, body = perform client ~sw (Uri.of_string url) in
          let status = Http.Status.to_int (Http.Response.status response) in
          let body_string =
            Eio.Buf_read.(parse_exn take_all) body ~max_size:max_response_size
          in
          Ok (classify_response status body_string))
      with
      | exn -> Ok (Error (`UnexpectedError (0, Printexc.to_string exn))))
  in
  match result with
  | Ok outcome -> outcome
  | Error `Timeout -> Error (`ClientError (408, "Request timed out"))
;;

let post ~env ?(timeout = 30.0) url body =
  let headers = Http.Header.init_with "Content-Type" "application/json" in
  request ~env ~timeout url (fun client ~sw uri ->
    Cohttp_eio.Client.post ~headers ~body:(Cohttp_eio.Body.of_string body) client ~sw uri)
;;

let get ~env ?(timeout = 10.0) url =
  request ~env ~timeout url (fun client ~sw uri -> Cohttp_eio.Client.get client ~sw uri)
;;
