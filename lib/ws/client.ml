module type Serializer = sig
  type message_in
  type message_out

  val serialize : message_out -> string
  val deserialize : string -> message_in
end

module Make (S : Serializer) = struct
  type t = Raw_client.t

  let connect = Raw_client.connect

  let receive_one (client : t) : S.message_in Lwt.t =
    let open Lwt.Infix in
    Raw_client.receive_one client >|= S.deserialize
  ;;

  let send_one (client : t) (message : S.message_out) : unit Lwt.t =
    let serialized = S.serialize message in
    Raw_client.send_one client serialized
  ;;

  let close = Raw_client.close

  let duplex
    (client : t)
    (receive : S.message_in -> unit Lwt.t)
    (send : unit -> [> `Close | `Message of S.message_out ] option Lwt_stream.t)
    : unit Lwt.t
    =
    let wrapped_send () =
      Lwt_stream.map
        (function
          | Some `Close -> Some `Close
          | Some (`Message msg) -> Some (`Message (S.serialize msg))
          | None -> None)
        (send ())
    in
    let wrapped_receive msg = receive (S.deserialize msg) in
    Raw_client.duplex client wrapped_receive wrapped_send
  ;;
end
