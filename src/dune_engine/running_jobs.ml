open Import
module Svar = Fiber.Svar
module Id = Stdune.Id.Make ()

type job =
  { pid : Pid.t
  ; description : unit Pp.t
  ; started_at : Time.t
  ; id : Id.t
  }

type event =
  | Start of job
  | Stop of Id.t

type t =
  { stamp : int
  ; current : job Id.Map.t
  ; last_event : event option
  }

let current t = t.current
let jobs = Svar.create { stamp = 0; current = Id.Map.empty; last_event = None }

let update event =
  let open Fiber.O in
  let* () = Fiber.return () in
  let { current; stamp; last_event = _ } = Svar.read jobs in
  let current =
    match event with
    | Start ({ id; _ } as job) ->
      if Id.Map.mem current id
      then Code_error.raise "running job started twice" [ "id", Id.to_dyn id ];
      Id.Map.set current id job
    | Stop id ->
      if not (Id.Map.mem current id)
      then
        Code_error.raise
          "running job stopped before it was started"
          [ "id", Id.to_dyn id ];
      Id.Map.remove current id
  in
  Svar.write jobs { current; stamp = stamp + 1; last_event = Some event }
;;

let start id pid ~description ~started_at =
  update (Start { pid; description; started_at; id })
;;

let stop id = update (Stop id)

let event_equal x y =
  match x, y with
  | Stop x, Stop y -> Id.equal x y
  | Start x, Start y -> Id.equal x.id y.id
  | _, _ -> false
;;

let equal x y =
  x.stamp = y.stamp
  && Option.equal event_equal x.last_event y.last_event
  && Id.Map.equal x.current y.current ~equal:(fun _ _ ->
    (* our job id's are unique *)
    true)
;;

let one_event_diff ~last ~now =
  if last.stamp + 1 = now.stamp then now.last_event else None
;;
