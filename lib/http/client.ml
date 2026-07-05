module type Serializer = sig
  type request
  type response

  val serialize_request : request -> string
  val deserialize_response : string -> response
end

module Make (S : Serializer) = struct
  let get ~env ?timeout (url : string) : (S.response, Raw_client.error) result =
    Raw_client.get ~env ?timeout url |> Result.map S.deserialize_response
  ;;

  let post ~env ?timeout (url : string) (req : S.request)
    : (S.response, Raw_client.error) result
    =
    Raw_client.post ~env ?timeout url (S.serialize_request req)
    |> Result.map S.deserialize_response
  ;;
end
