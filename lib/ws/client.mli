(** Module type defining serialization/deserialization requirements *)
module type Serializer = sig
  (** Type of messages received from the WebSocket *)
  type message_in

  (** Type of messages sent to the WebSocket *)
  type message_out

  (** [serialize msg] converts an outgoing message to a string *)
  val serialize : message_out -> string

  (** [deserialize str] converts an incoming string to a message *)
  val deserialize : string -> message_in
end

(** Functor to create a typed WebSocket client *)
module Make (S : Serializer) : sig
  (** Type of the WebSocket client *)
  type t

  (** [connect uri] connects to a WebSocket server at the given URI *)
  val connect : Uri.t -> t Lwt.t

  (** [receive_one client] receives a single message from the WebSocket *)
  val receive_one : t -> S.message_in Lwt.t

  (** [send_one client message] sends a single message to the WebSocket *)
  val send_one : t -> S.message_out -> unit Lwt.t

  (** [close client] closes the WebSocket connection *)
  val close : t -> unit Lwt.t

  (** [duplex client receive send] sets up bidirectional communication
      where [receive] processes incoming messages and [send] provides outgoing messages *)
  val duplex
    :  t ->
    (S.message_in -> unit Lwt.t) ->
    (unit -> [ `Close | `Message of S.message_out ] option Lwt_stream.t) ->
    unit Lwt.t
end
