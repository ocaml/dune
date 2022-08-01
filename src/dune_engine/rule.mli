(** Representation of rules *)

open Import
module Action_builder := Action_builder0

(** Information about the provenance of a build rule. *)
module Info : sig
  type t =
    | From_dune_file of Loc.t
    | Internal
    | Source_file_copy of Path.Source.t

  val of_loc_opt : Loc.t option -> t
end

module Promote : sig
  module Lifetime : sig
    type t =
      | Unlimited
      | Until_clean  (** The promoted file will be deleted by [dune clean] *)
  end

  module Into : sig
    type t =
      { loc : Loc.t
      ; dir : string
      }
  end

  type t =
    { lifetime : Lifetime.t
    ; into : Into.t option
    ; only : Filename.t Predicate.t option
    }
end

module Mode : sig
  type t =
    | Standard
    | Fallback  (** Only use this rule if the source files don't exist. *)
    | Promote of Promote.t
        (** Silently promote the targets to the source tree. *)
    | Ignore_source_files
        (** Just ignore the source files entirely. This is for cases where the
            targets are promoted only in a specific context, such as for
            .install files. *)
end

module Id : sig
  type t

  val compare : t -> t -> Ordering.t

  include Comparable_intf.S with type key := t
end

type t = private
  { id : Id.t
  ; context : Build_context.t option
  ; targets : Targets.Validated.t
  ; action : Action.Full.t Action_builder.t
  ; mode : Mode.t
  ; info : Info.t
  ; loc : Loc.t
  ; dir : Path.Build.t  (** Directory where all the targets are produced. *)
  }

include Comparable_intf.S with type key := t

val equal : t -> t -> bool

val hash : t -> int

val to_dyn : t -> Dyn.t

(** [make] raises an error if the set of [targets] is not well-formed. See the
    [Targets.Validation_result] data type for the list of possible problems. *)
val make :
     ?mode:Mode.t
  -> context:Build_context.t option
  -> ?info:Info.t
  -> targets:Targets.t
  -> Action.Full.t Action_builder.t
  -> t

val set_action : t -> Action.Full.t Action_builder.t -> t

val loc : t -> Loc.t

(** [find_source_dir rule] is the closest source directory corresponding to
    rule.dir. Eg. [src/dune] for a rule with dir
    [_build/default/src/dune/.dune.objs]. *)
val find_source_dir : t -> Source_tree.Dir.t Memo.t

module Anonymous_action : sig
  (* jeremiedimino: this type correspond to a subset of [Rule.t]. We should
     eventually share the code. *)
  type t =
    { context : Build_context.t option
    ; action : Action.Full.t
    ; loc : Loc.t option
    ; dir : Path.Build.t
          (** Directory the action is attached to. This is the directory where
              the outcome of the action will be cached. *)
    ; alias : Alias.Name.t option  (** For better error messages *)
    }
end
