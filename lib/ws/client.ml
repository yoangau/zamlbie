module type Serializer = sig
  type message_in
  type message_out

  val serialize : message_out -> string
  val deserialize : string -> message_in
end

module Make (S : Serializer) = struct
  type t = Raw_client.t

  let connect = Raw_client.connect

  let receive_one (client : t) : S.message_in =
    Raw_client.receive_one client |> S.deserialize
  ;;

  let send_one (client : t) (message : S.message_out) : unit =
    Raw_client.send_one client (S.serialize message)
  ;;

  let close = Raw_client.close
end
