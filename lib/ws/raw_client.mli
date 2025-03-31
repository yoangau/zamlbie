type t

(** [connect uri] establishes a WebSocket connection to the given [uri]. *)
val connect : Uri.t -> t Lwt.t

(** [duplex t receive send] sets up bidirectional communication for the WebSocket client.
    - [receive] is a function handling incoming messages.
    - [send] is a stream producing messages to be sent. *)
val duplex
  :  t ->
  (string -> unit Lwt.t) ->
  (unit -> [> `Close | `Message of string ] option Lwt_stream.t) ->
  unit Lwt.t

(** [receive_one t] waits for and returns a single message from the WebSocket connection. *)
val receive_one : t -> string Lwt.t

(** [send_one ws message] sends a text message over the WebSocket connection. *)
val send_one : t -> string -> unit Lwt.t

(** [close ws] sends a close signal to terminate the WebSocket connection. *)
val close : t -> unit Lwt.t
