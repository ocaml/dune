open Stdune
open Core

type status =
  | Open (* new tasks are allowed *)
  | Closed (* new tasks are forbidden *)

type runner =
  | Running (* Firing fibers inside the queue. *)
  | Awaiting_resume of unit k
    (* Ran out of work. Waiting to be resumed once work is added or
       pool is closed. *)
  | Awaiting_run (* Just created. [run] hasn't been called yet. *)

(* A pool consumes tasks from a queue in the context where [run] was executed.

   It's implemented by a simple queue of thunks and a continuation to resume
   [run] whenever it runs out of work.

   To optimize this further, we can bake in the operation into [effect] in [Core]. *)

type nonrec t =
  { tasks : (unit -> unit t) Queue.t (* pending tasks *)
  ; mutable runner : runner (* The continuation to resume the runner set by [run] *)
  ; mutable status : status
  }

let running t k =
  match t.status with
  | Open -> k true
  | Closed -> k false
;;

let create () = { tasks = Queue.create (); runner = Awaiting_run; status = Open }

let task t ~f k =
  match t.status with
  | Closed -> Code_error.raise "pool is closed. new tasks may not be submitted" []
  | Open ->
    Queue.push t.tasks f;
    (match t.runner with
     | Running | Awaiting_run -> k ()
     | Awaiting_resume r ->
       t.runner <- Running;
       resume r () k)
;;

let close t k =
  match t.status with
  | Closed -> k ()
  | Open ->
    t.status <- Closed;
    (match t.runner with
     | Running | Awaiting_run -> k ()
     | Awaiting_resume r ->
       t.runner <- Running;
       resume r () k)
;;

let run t k =
  match t.runner with
  | Awaiting_resume _ | Running ->
    Code_error.raise "Fiber.Pool.run: concurent calls to run aren't allowed" []
  | Awaiting_run ->
    t.runner <- Running;
    (* The number of currently running fibers in the pool. Only when this
       number reaches zero we may call the final continuation [k]. *)
    let n = ref 1 in
    let done_fiber () =
      decr n;
      if !n = 0 then k () else end_of_fiber
    in
    let rec read t =
      match Queue.pop t.tasks with
      | None -> finish_or_suspend t
      | Some v ->
        incr n;
        fork (fun () -> v () done_fiber) read_delayed
    and read_delayed () = read t
    and suspend_k k =
      (* we are suspending because we have no tasks *)
      assert (Queue.is_empty t.tasks);
      t.runner <- Awaiting_resume k
    and finish_or_suspend t =
      match t.status with
      | Closed -> done_fiber ()
      | Open -> suspend suspend_k read_delayed
    in
    read t
;;
