open Base

(* A match is an actor: a single orchestrator fiber (running on one of the
   match-runner domains) exclusively owns the mutable game state and the
   [players] table. Everything else — websocket sessions running on the
   accept domains, the HTTP handlers — communicates with it exclusively
   through domain-safe primitives: the [inbox] event stream, per-player
   [Atomic.t] move mailboxes and per-player outbox streams. *)

module Player = struct
  type t =
    { outbox : Message.server_message Eio.Stream.t;
      mailbox : Game.WireFormat.move option Atomic.t
    }

  let outbox_capacity = 64
  let make () = { outbox = Eio.Stream.create outbox_capacity; mailbox = Atomic.make None }

  (* Called from the player's websocket session (any domain). Only the last
     unprocessed move matters, so a single-slot atomic mailbox is enough. *)
  let mail move player = Atomic.set player.mailbox (Some move)

  (* Called by the orchestrator on each tick. *)
  let take_mail player = Atomic.exchange player.mailbox None

  (* Best-effort send that never blocks the orchestrator. The orchestrator is
     the only producer on [outbox] and the session fiber only ever removes
     elements, so the length check cannot race into a blocking [add]: a full
     outbox (stuck or dead client) simply drops updates. *)
  let send player message =
    if Eio.Stream.length player.outbox < outbox_capacity
    then Eio.Stream.add player.outbox message
  ;;

  (* Called from the player's websocket session; blocks until the
     orchestrator sends something. *)
  let receive player = Eio.Stream.take player.outbox
end

type joined =
  { player_id : Uuid.HashtblKey.t;
    player : Player.t
  }

type event =
  | Join of { reply : (joined, string) Result.t Eio.Promise.u }
  | Disconnect of Uuid.HashtblKey.t

type t =
  { match_id : int;
    inbox : event Eio.Stream.t;
    (* Orchestrator-owned: never touch from another fiber. *)
    players : (Uuid.HashtblKey.t, Player.t) Hashtbl.t;
    started : unit Eio.Promise.t * unit Eio.Promise.u;
    (* Mirror of [Hashtbl.length players] readable from any domain (lobby
       listings). *)
    player_count : int Atomic.t;
    (* Orchestrator-owned. *)
    mutable state : Game.t
  }

let inbox_capacity = 64
let update_game_state t new_state = t.state <- new_state
let player_count t = Atomic.get t.player_count
let is_started t = Eio.Promise.is_resolved (fst t.started)

(* Safe from any domain. *)
let post t event = Eio.Stream.add t.inbox event

(* Orchestrator side. *)
let next_event t = Eio.Stream.take t.inbox
let poll_event t = Eio.Stream.take_nonblocking t.inbox
let players_iter t ~f = Hashtbl.iteri t.players ~f:(fun ~key ~data -> f key data)
let broadcast t message = players_iter t ~f:(fun _ player -> Player.send player message)

let start t =
  let start_game_state = Effects.(apply Start.effects t.state) in
  update_game_state t start_game_state;
  Eio.Promise.resolve (snd t.started) ()
;;

let try_join t =
  let room_size = t.state.config.max_player_count in
  if is_started t
  then Error "Game already started!"
  else if player_count t >= room_size
  then Error "Game full!"
  else (
    let player_id, game =
      Game.add_entity t.state { Game.default_entity with entity_type = `Player `Human }
    in
    t.state <- game;
    let player = Player.make () in
    Hashtbl.set t.players ~key:player_id ~data:player;
    Atomic.incr t.player_count;
    if player_count t = room_size then start t;
    Ok { player_id; player })
;;

let disconnect t player_id =
  if Hashtbl.mem t.players player_id
  then (
    ignore (Game.remove_entity t.state player_id : Game.t);
    Hashtbl.remove t.players player_id;
    Atomic.decr t.player_count)
;;

module Registry = struct
  (* The registry is shared by every accept domain; all table accesses are
     serialized by [mutex]. *)
  let mutex = Stdlib.Mutex.create ()
  let matches = Hashtbl.create (module Uuid.HashtblKey)
  let next_id_gen = Uuid.create_gen ()
  let with_lock f = Stdlib.Mutex.protect mutex f
  let find id = with_lock (fun () -> Hashtbl.find matches id)
  let remove id = with_lock (fun () -> Hashtbl.remove matches id)

  let new_match config =
    let match_id = Uuid.next_id next_id_gen in
    let game_match =
      { match_id;
        inbox = Eio.Stream.create inbox_capacity;
        players = Hashtbl.create (module Uuid.HashtblKey);
        started = Eio.Promise.create ();
        player_count = Atomic.make 0;
        state = Game.make match_id config
      }
    in
    with_lock (fun () -> Hashtbl.add_exn matches ~key:match_id ~data:game_match);
    game_match
  ;;

  let list_waiting_matches () =
    let snapshot = with_lock (fun () -> Hashtbl.to_alist matches) in
    List.filter_map snapshot ~f:(fun (match_id, game_match) ->
      if is_started game_match
      then None
      else (
        let current_players = player_count game_match in
        (* [config] is immutable and carried unchanged through every state
           update, so reading it from another domain is safe. *)
        let config = game_match.state.config in
        let config_preview =
          Printf.sprintf
            "%dx%d, %d floors"
            config.width
            config.height
            config.number_of_floor
        in
        Some
          Game.WireFormat.
            { game_id = match_id;
              current_players;
              max_players = config.max_player_count;
              config_preview
            }))
  ;;
end
