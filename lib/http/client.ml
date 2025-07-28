module type Serializer = sig
  type request
  type response

  val serialize_request : request -> string
  val deserialize_response : string -> response
end

module Make (S : Serializer) = struct
  type t = unit

  let request (url : string) (req : S.request option)
    : (S.response, Raw_client.error) result Lwt.t
    =
    let open Lwt.Infix in
    match req with
    | None -> Raw_client.get url >|= Result.map S.deserialize_response
    | Some req ->
      let serialized = S.serialize_request req in
      Raw_client.post url serialized >|= Result.map S.deserialize_response
  ;;

  let get (url : string) : (S.response, Raw_client.error) result Lwt.t = request url None

  let post (url : string) (req : S.request) : (S.response, Raw_client.error) result Lwt.t =
    request url (Some req)
  ;;
end
