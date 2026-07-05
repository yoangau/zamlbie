type t

(** Connect to a [ws://] or [wss://] (also accepts [http(s)://]) endpoint and
    perform the websocket handshake. The connection lives until [sw] is
    released. Raises [Failure] if the endpoint cannot be reached or rejects
    the handshake. *)
val connect : sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> Uri.t -> t

(** Wait for the next complete text/binary message.
    @raise End_of_file once the connection is closed. *)
val receive_one : t -> string

val send_one : t -> string -> unit
val close : t -> unit
