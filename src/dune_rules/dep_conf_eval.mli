(** Interpret dependencies written in Dune files *)
open! Stdune

open! Dune_engine

(** Evaluates unnamed dependency specifications. *)
val unnamed : expander:Expander.t -> Dep_conf.t list -> unit Action_builder.t

<<<<<<< HEAD
(** Evaluates named dependency specifications. Return the action build that
    register dependencies as well as an expander that can be used to expand to
    expand variables from the bindings. *)
||||||| parent of ff33e0c28... depend on stamp files
(** Evaluates to the actual list of dependencies, ignoring aliases, and
    registers them as the action dependencies.

    It returns bindings that are later used for action expansion. *)
=======
(** A variant specifying what set of paths should aliases be expand into. *)
module Alias_expansion : sig
  type t =
    | Empty (** Expand into an empty set of paths. *)
    | Stamp_file (** Expand into the alias stamp file. *)
      (* CR-someday aalekseyev:
         We should add a Full mode where the aliases are expanded to the list of
         files contained within. *)
end

(** Evaluates to the actual list of dependencies, and registers them as the action
    dependencies.

    Aliases may or may not contribute to the final list of files, as specified by
    [alias_expansion]. Regardless of [alias_expansion], the dependency on alias
    stamp file is registered.

    It returns bindings that are later used for action expansion. *)
>>>>>>> ff33e0c28... depend on stamp files
val named :
  alias_expansion:Alias_expansion.t
  -> expander:Expander.t
  -> Dep_conf.t Bindings.t
  -> unit Action_builder.t * Expander.t
