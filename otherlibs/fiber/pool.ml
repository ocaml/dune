open Stdune
open Core

type task =
  | Done
  | Task of (unit -> unit t)

type status =
  | Open
  | Closed

type t =
  { writers : (task * unit k) Queue.t
  ; mutable reader : task k option
  ; mutable value : task option
  ; mutable status : status
  }

let running t k =
  match t.status with
  | Open -> k true
  | Closed -> k false

let create () =
  { value = None; writers = Queue.create (); reader = None; status = Open }

let write t x k =
  match t.value with
  | Some _ -> suspend (fun k -> Queue.push t.writers (x, k)) k
  | None -> (
    match t.reader with
    | None ->
      t.value <- Some x;
      k ()
    | Some r ->
      t.reader <- None;
      resume r x (fun () -> k ()))

let task t ~f k =
  match t.status with
  | Closed ->
    Code_error.raise "pool is closed. new tasks may not be submitted" []
  | Open -> write t (Task f) k

let stop t k =
  match t.status with
  | Closed -> k ()
  | Open ->
    t.status <- Closed;
    write t Done k

let read t k =
  match t.value with
  | None ->
    suspend
      (fun k ->
        assert (t.reader = None);
        t.reader <- Some k)
      k
  | Some v -> (
    match Queue.pop t.writers with
    | None ->
      t.value <- None;
      k v
    | Some (v', w) ->
      t.value <- Some v';
      resume w () (fun () -> k v))

let run t k =
  let n = ref 1 in
  let k () =
    decr n;
    if !n = 0 then k () else end_of_fiber
  in
  let rec loop t =
    read t (function
      | Done -> k ()
      | Task x ->
        incr n;
        fork (fun () -> x () k) (fun () -> loop t))
  in
  loop t
