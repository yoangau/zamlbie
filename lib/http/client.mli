module type Serializer = sig
  type request
  type response

  val serialize_request : request -> string
  val deserialize_response : string -> response
end

module Make (S : Serializer) : sig
  type t = unit

  val get : string -> (S.response, Raw_client.error) result Lwt.t
  val post : string -> S.request -> (S.response, Raw_client.error) result Lwt.t
end
