(** Action builder *)

open! Import
include module type of Action_builder0

(** Record the given set as dependencies of the action produced by the action builder. *)
val record : Dep.Set.t -> unit t

(** [contents path] returns an action builder that when run will evaluate to the contents
    of the file at [path]. *)
val contents : Path.t -> string t

(** [if_file_exists file ~then ~else] evaluates to [then_] if the [file] is present in the
    source tree or is a target of a rule. Otherwise, it evaluates to [else_]. *)
val if_file_exists : Path.t -> then_:'a t -> else_:'a t -> 'a t
