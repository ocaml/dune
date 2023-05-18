open Import

(** The default environment in which to run commands. The environment of
    individual build contexts augment this environment. *)
val env : unit -> Env.t

(** Initialises this module. *)
val init : Env.t -> unit
