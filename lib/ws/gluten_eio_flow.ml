(*----------------------------------------------------------------------------
 *  Copyright (c) 2022 António Nuno Monteiro
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *  this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *  notice, this list of conditions and the following disclaimer in the
 *  documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the name of the copyright holder nor the names of its
 *  contributors may be used to endorse or promote products derived from this
 *  software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 *  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 *  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 *  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 *  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*)

(* Vendored from gluten-eio 0.5.2 (gluten_eio.ml), with the client runtime
   generalized to accept any [Eio.Flow.two_way] instead of a concrete
   [Eio.Net.stream_socket]. This lets us drive the websocket state machine
   over a TLS flow ([Tls_eio.t]) as well as a plain TCP socket, which the
   upstream signature does not allow. Only the client half is kept; the
   server uses stock httpun-eio. *)

open Eio.Std
module Buffer = Gluten.Buffer

module IO_loop = struct
  let writev socket iovecs =
    let lenv, cstructs =
      List.fold_left_map
        (fun acc { Faraday.buffer; off; len } ->
           (acc + len, Cstruct.of_bigarray buffer ~off ~len))
        0
        iovecs
    in
    match Eio.Flow.write socket cstructs with
    | () -> `Ok lenv
    | exception End_of_file -> `Closed
  ;;

  let read_once flow buffer =
    let p, u = Promise.create () in
    Buffer.put
      ~f:(fun buf ~off ~len k ->
        let cstruct = Cstruct.of_bigarray buf ~off ~len in
        k (Eio.Flow.single_read flow cstruct))
      buffer
      (Promise.resolve u);
    Promise.await p
  ;;

  let read flow buffer =
    match read_once flow buffer with
    | r -> r
    | exception
        ( Unix.Unix_error (ENOTCONN, _, _)
        | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (ENOTCONN, _, _)), _)
        | Eio.Io (Eio.Net.E (Connection_reset _), _) ) -> raise End_of_file
  ;;

  let shutdown flow cmd =
    try Eio.Flow.shutdown flow cmd with
    | Unix.Unix_error (ENOTCONN, _, _)
    | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (ENOTCONN, _, _)), _) -> ()
  ;;

  let start
    : type t.
      (module Gluten.RUNTIME with type t = t) ->
      read_buffer_size:int ->
      read_closed:unit Promise.t * unit Promise.u ->
      t ->
      _ Eio.Flow.two_way ->
      unit
    =
    fun (module Runtime) ~read_buffer_size ~read_closed t socket ->
    let read_closed, resolve_read_closed = read_closed
    and write_closed = ref false in
    (* Deviation from upstream: reporting an exception during connection
       teardown can itself raise (httpun-ws's [report_exn] tries to write a
       Close frame, which fails if the writer is already closed). The
       connection is dying either way; don't let the secondary failure
       escape and take the caller's switch down with it. *)
    let report_exn t exn =
      try Runtime.report_exn t exn with
      | Failure _ -> ()
    in
    let read_buffer = Buffer.create read_buffer_size in
    let rec read_loop =
      let read socket read_buffer =
        Fiber.first
          (fun () -> read socket read_buffer)
          (fun () ->
             Promise.await read_closed;
             raise End_of_file)
      in
      fun () ->
        let rec read_loop_step () =
          match Runtime.next_read_operation t with
          | `Read ->
            (match read socket read_buffer with
             | _n ->
               let (_ : int) =
                 Buffer.get read_buffer ~f:(fun buf ~off ~len ->
                   Runtime.read t buf ~off ~len)
               in
               ()
             | exception End_of_file ->
               let (_ : int) =
                 Buffer.get read_buffer ~f:(fun buf ~off ~len ->
                   Runtime.read_eof t buf ~off ~len)
               in
               ());
            read_loop_step ()
          | `Yield ->
            let p, u = Promise.create () in
            Runtime.yield_reader t (fun () -> Promise.resolve u ());
            Promise.await p;
            read_loop ()
          | `Close ->
            (match Promise.is_resolved read_closed with
             | true -> ()
             | false ->
               (match read socket read_buffer with
                | _n -> assert false
                | exception (End_of_file as exn) ->
                  shutdown socket `Receive;
                  Promise.resolve resolve_read_closed ();
                  (match !write_closed with
                   | true -> ()
                   | false -> report_exn t exn)))
        in
        match read_loop_step () with
        | () -> ()
        | exception exn -> report_exn t exn
    in
    let rec write_loop () =
      let rec write_loop_step () =
        match Runtime.next_write_operation t with
        | `Write io_vectors ->
          let write_result = writev socket io_vectors in
          Runtime.report_write_result t write_result;
          write_loop_step ()
        | `Yield ->
          let p, u = Promise.create () in
          Runtime.yield_writer t (fun () -> Promise.resolve u ());
          Promise.await p;
          write_loop ()
        | `Close _ ->
          write_closed := true;
          shutdown socket `Send
      in
      match write_loop_step () with
      | () -> ()
      | exception exn -> report_exn t exn
    in
    Fiber.both read_loop write_loop
  ;;
end

module Client = struct
  type t =
    { connection : Gluten.Client.t;
      shutdown_reader : unit -> unit;
      shutdown_complete : unit Promise.t
    }

  let create ~sw ~read_buffer_size ~protocol t socket =
    let connection = Gluten.Client.create ~protocol t in
    let shutdown_p, shutdown_u = Promise.create () in
    let read_closed = Promise.create () in
    Fiber.fork ~sw (fun () ->
      Fun.protect ~finally:(Promise.resolve shutdown_u) (fun () ->
        try
          Switch.run (fun sw ->
            Fiber.fork ~sw (fun () ->
              IO_loop.start
                (module Gluten.Client)
                ~read_closed
                ~read_buffer_size
                connection
                socket))
        with
        (* Deviation from upstream: a teardown error must not propagate into
           the caller's switch, where it would cancel unrelated fibers (e.g.
           other connections). The reader has already observed eof by now. *)
        | exn -> Printf.eprintf "websocket runtime error: %s\n%!" (Printexc.to_string exn)));
    { connection;
      shutdown_reader =
        (fun () ->
          let cancel_reader, resolve_cancel_reader = read_closed in
          if not (Promise.is_resolved cancel_reader)
          then Promise.resolve resolve_cancel_reader ());
      shutdown_complete = shutdown_p
    }
  ;;

  let upgrade t protocol = Gluten.Client.upgrade_protocol t.connection protocol

  let shutdown t =
    t.shutdown_reader ();
    Gluten.Client.shutdown t.connection;
    t.shutdown_complete
  ;;

  let is_closed t = Gluten.Client.is_closed t.connection
end
