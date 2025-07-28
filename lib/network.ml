module WsClient = Ws.Client.Make (struct
    type message_in = Message.server_message
    type message_out = Message.client_message

    let serialize mess = Message.string_of_client_message mess
    let deserialize = Message.server_message_of_string
  end)

module HttpClient = Http.Client.Make (struct
    type request = Message.http_request
    type response = Message.http_response

    let serialize_request req = Message.string_of_http_request req
    let deserialize_response = Message.http_response_of_string
  end)
