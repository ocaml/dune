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

let run t k =
  let n = ref 1 in
  let done_fiber () =
    decr n;
    if !n = 0 then k () else end_of_fiber
  in
  let rec read t =
    match t.value with
    | None -> on_finish t
    | Some v -> (
      match Queue.pop t.writers with
      | None ->
        t.value <- None;
        on_read v
      | Some (v', w) ->
        t.value <- Some v';
        resume w () (fun () -> on_read v))
  and suspend_k k =
    assert (t.reader = None);
    t.reader <- Some k
  and on_finish t =
    match t.status with
    | Closed -> done_fiber ()
    | Open -> suspend suspend_k on_read
  and read_delayed () = read t
  and on_read = function
    | Done -> done_fiber ()
    | Task x ->
      incr n;
      fork (fun () -> x () done_fiber) read_delayed
  in
  read t
