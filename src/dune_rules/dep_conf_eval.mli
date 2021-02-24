(** Interpret dependencies written in Dune files *)
open! Stdune

open! Dune_engine

(** Evaluates unnamed dependency specifications. *)
val unnamed : expander:Expander.t -> Dep_conf.t list -> unit Action_builder.t

(** A variant specifying what set of paths should aliases be expand into. *)
module Alias_expansion : sig
  type t =
    | Empty (** Expand into an empty set of paths. *)
    | Stamp_file (** Expand into the alias stamp file. *)
      (* CR-someday aalekseyev:
         We should add a Full mode where the aliases are expanded to the list of
         files contained within. *)
end

(** TODO: get rid of this somehow, making alias expansions work in a more
    straightforward manner. *)
val named0 :
  alias_expansion:Alias_expansion.t
  -> expander:Expander.t
  -> Dep_conf.t Bindings.t
  ->
  Path.t list Action_builder.t *
  Dune_util.Value.t list Action_builder.t
    Pform.Map.t

(** Evaluates named dependency specifications. Return the action build that
    register dependencies as well as an expander that can be used to expand to
    expand variables from the bindings.

    Aliases may or may not contribute to the final list of files, as specified by
    [alias_expansion]. Regardless of [alias_expansion], the dependency on alias
    stamp file is registered.

    It returns bindings that are later used for action expansion. *)
val named :
  alias_expansion:Alias_expansion.t
  -> expander:Expander.t
  -> Dep_conf.t Bindings.t
  -> unit Action_builder.t * Expander.t
