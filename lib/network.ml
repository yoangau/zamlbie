module TypedSerializer = struct
  type message_in = Message.server_message
  type message_out = Message.client_message

  let serialize mess = Message.string_of_client_message mess
  let deserialize = Message.server_message_of_string
end

module WsClient = Ws.Client.Make (TypedSerializer)
