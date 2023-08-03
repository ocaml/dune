open Import
module Svar = Fiber.Svar
module Id = Stdune.Id.Make ()

type job =
  { pid : Pid.t
  ; description : unit Pp.t
  ; started_at : float
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

let start id pid ~description ~started_at =
  let new_jobs =
    let jobs = Svar.read jobs in
    let job = { pid; description; started_at; id } in
    let current = Id.Map.set jobs.current id job in
    { current; stamp = jobs.stamp + 1; last_event = Some (Start job) }
  in
  Svar.write jobs new_jobs
;;

let stop id =
  let new_jobs =
    let jobs = Svar.read jobs in
    let current = Id.Map.remove jobs.current id in
    { current; stamp = jobs.stamp + 1; last_event = Some (Stop id) }
  in
  Svar.write jobs new_jobs
;;

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
