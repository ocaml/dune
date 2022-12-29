open Stdune
open Core
open Core.O

type mvar =
  | Done
  | Task of (unit -> unit t)

module Mvar = struct
  type t =
    { writers : (mvar * unit k) Queue.t
    ; readers : mvar k Queue.t
    ; mutable value : mvar option
    }

  let create () =
    { value = None; writers = Queue.create (); readers = Queue.create () }

  let read t k =
    match t.value with
    | None -> suspend (fun k -> Queue.push t.readers k) k
    | Some v -> (
      match Queue.pop t.writers with
      | None ->
        t.value <- None;
        k v
      | Some (v', w) ->
        t.value <- Some v';
        resume w () (fun () -> k v))

  let write t x k =
    match t.value with
    | Some _ -> suspend (fun k -> Queue.push t.writers (x, k)) k
    | None -> (
      match Queue.pop t.readers with
      | None ->
        t.value <- Some x;
        k ()
      | Some r -> resume r x (fun () -> k ()))
end

type status =
  | Open
  | Closed

type t =
  { mvar : Mvar.t
  ; mutable status : status
  }

let running t k =
  match t.status with
  | Open -> k true
  | Closed -> k false

let create () = { mvar = Mvar.create (); status = Open }

let task t ~f k =
  match t.status with
  | Closed ->
    Code_error.raise "pool is closed. new tasks may not be submitted" []
  | Open -> Mvar.write t.mvar (Task f) k

let stream t =
  Stream.In.create (fun () ->
      let+ next = Mvar.read t.mvar in
      match next with
      | Done -> None
      | Task task -> Some task)

let stop t k =
  match t.status with
  | Closed -> k ()
  | Open ->
    t.status <- Closed;
    Mvar.write t.mvar Done k

let run t = stream t |> Stream.In.parallel_iter ~f:(fun task -> task ())
