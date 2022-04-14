open Stdune
open Core
open Core.O

type mvar =
  | Done
  | Task of (unit -> unit t)

type status =
  | Open
  | Closed

type t =
  { mvar : mvar Mvar.t
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
