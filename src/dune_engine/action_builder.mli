(** Action builder *)

open! Import
include module type of Action_builder0

(** Record the given set as dependencies of the action produced by the action builder. *)
val record : Dep.Set.t -> unit t

(** Evaluate a file selector and record all the matched files as dependencies of the
    action produced by the action builder. *)
val paths_matching : loc:Loc.t -> File_selector.t -> unit t

(** [contents path] returns an action builder that when run will evaluate to the contents
    of the file at [path]. *)
val contents : Path.t -> string t

(** Evaluates to [true] if the file is present in the source tree or is a target of a
    rule. It doesn't add the path as dependency of the resulting action builder. *)
val file_exists : Path.t -> bool t

(** [if_file_exists p ~then ~else] evaluates to [then_] if [file_exists p] evaluates to
    [true], and [else_] otherwise. *)
val if_file_exists : Path.t -> then_:'a t -> else_:'a t -> 'a t
