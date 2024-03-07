(** State to track the currently running jobs *)

open Import

(** Unique job id *)
module Id : Id.S

(** Job description *)
type job =
  { pid : Pid.t
  ; description : unit Pp.t
  ; started_at : float
  ; id : Id.t
  }

(** The possible events we can get to update the job state *)
type event =
  | Start of job
  | Stop of Id.t

type t

val current : t -> job Id.Map.t
val start : Id.t -> Pid.t -> description:unit Pp.t -> started_at:float -> unit Fiber.t
val stop : Id.t -> unit Fiber.t
val equal : t -> t -> bool

(** If two jobs states differ by a single event, then return it. *)
val one_event_diff : last:t -> now:t -> event option

(** The state variable for jobs *)
val jobs : t Fiber.Svar.t
