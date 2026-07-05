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

  (** [connect ~sw ~env uri] connects to a WebSocket server at the given URI *)
  val connect : sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> Uri.t -> t

  (** [receive_one client] waits for a single message from the WebSocket.
      @raise End_of_file once the connection is closed. *)
  val receive_one : t -> S.message_in

  (** [send_one client message] sends a single message to the WebSocket *)
  val send_one : t -> S.message_out -> unit

  (** [close client] closes the WebSocket connection *)
  val close : t -> unit
end
