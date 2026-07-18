module type Serializer = sig
  type request
  type response

  val serialize_request : request -> string
  val deserialize_response : string -> response
end

module Make (S : Serializer) : sig
  val get
    :  env:Eio_unix.Stdenv.base ->
    ?timeout:float ->
    string ->
    (S.response, Raw_client.error) result

  val post
    :  env:Eio_unix.Stdenv.base ->
    ?timeout:float ->
    string ->
    S.request ->
    (S.response, Raw_client.error) result
end
