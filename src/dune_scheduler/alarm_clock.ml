open Stdune
module Thread = Thread0

type alarm = [ `Finished | `Cancelled ] Fiber.Ivar.t

type t =
  { events : Event.Queue.t
  ; mutex : Mutex.t
  ; period_seconds : Time.Span.t
  ; mutable alarms : (Time.t * [ `Finished | `Cancelled ] Fiber.Ivar.t) list
  ; mutable active : bool
  }

let await = Fiber.Ivar.read

let cancel t alarm =
  Mutex.lock t.mutex;
  let found = ref false in
  t.alarms
  <- List.filter t.alarms ~f:(fun (_, alarm') ->
       let eq = alarm' == alarm in
       if eq then found := true;
       not eq);
  Mutex.unlock t.mutex;
  if !found
  then Event.Queue.send_timers_completed t.events [ Fiber.Fill (alarm, `Cancelled) ]
;;

let polling_loop t () =
  let rec loop () =
    match t.active with
    | false -> ()
    | true ->
      let now = Time.now () in
      let expired, active =
        List.partition_map t.alarms ~f:(fun (expiration, ivar) ->
          if Time.( > ) now expiration
          then Left (Fiber.Fill (ivar, `Finished))
          else Right (expiration, ivar))
      in
      t.alarms <- active;
      Mutex.unlock t.mutex;
      (match Nonempty_list.of_list expired with
       | None -> ()
       | Some expired -> Event.Queue.send_timers_completed t.events expired);
      Thread.delay (Time.Span.to_secs t.period_seconds);
      Mutex.lock t.mutex;
      loop ()
  in
  Mutex.lock t.mutex;
  loop ();
  t.alarms <- [];
  Mutex.unlock t.mutex
;;

let create events period_seconds =
  let t =
    { events; active = true; alarms = []; period_seconds; mutex = Mutex.create () }
  in
  let (_ : Thread.t) = Thread.spawn (polling_loop t) in
  t
;;

let sleep t seconds =
  Mutex.lock t.mutex;
  let ivar = Fiber.Ivar.create () in
  if not t.active
  then (
    Mutex.unlock t.mutex;
    Code_error.raise "cannot schedule timers after close" []);
  t.alarms <- (Time.add (Time.now ()) seconds, ivar) :: t.alarms;
  Mutex.unlock t.mutex;
  ivar
;;

let close t =
  Mutex.lock t.mutex;
  t.active <- false;
  Mutex.unlock t.mutex
;;
