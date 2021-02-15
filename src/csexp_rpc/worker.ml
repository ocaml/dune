open Stdune

let with_mutex t ~f =
  Mutex.lock t;
  let res = f () in
  Mutex.unlock t;
  res

type state =
  | Running
  | Stopped
  | Finished

type 'a t =
  { work : 'a Queue.t
  ; mutable state : state
  ; mutex : Mutex.t
  ; work_available : Condition.t
  }

let is_running t =
  match t.state with
  | Running -> true
  | Stopped
  | Finished ->
    false

let run (f, t) =
  let rec loop () =
    match t.state with
    | Stopped -> (
      match Queue.pop t.work with
      | None -> t.state <- Finished
      | Some job -> do_work job )
    | Finished -> ()
    | Running -> (
      match Queue.pop t.work with
      | Some job -> do_work job
      | None ->
        while Queue.is_empty t.work && is_running t do
          Condition.wait t.work_available t.mutex
        done;
        loop () )
  and do_work job =
    Mutex.unlock t.mutex;
    f job;
    Mutex.lock t.mutex;
    loop ()
  in
  Mutex.lock t.mutex;
  loop ();
  Mutex.unlock t.mutex

let create ~spawn do_ =
  let t =
    { work = Queue.create ()
    ; state = Finished
    ; mutex = Mutex.create ()
    ; work_available = Condition.create ()
    }
  in
  t.state <- Running;
  spawn (fun () -> run (do_, t));
  t

let add_work (type a) (t : a t) (w : a) =
  with_mutex t.mutex ~f:(fun () ->
      if is_running t then (
        Queue.push t.work w;
        Condition.signal t.work_available;
        Ok ()
      ) else
        Error `Stopped)

let stop (t : _ t) =
  with_mutex t.mutex ~f:(fun () ->
      match t.state with
      | Running ->
        t.state <- Stopped;
        Condition.signal t.work_available
      | Stopped
      | Finished ->
        ())
