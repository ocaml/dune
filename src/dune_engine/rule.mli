(** Representation of rules *)

open! Stdune
open! Import

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
      | Unlimited  (** The promoted file will be deleted by [dune clean] *)
      | Until_clean
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
    ; only : Predicate_lang.Glob.t option
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

  module Map : Map.S with type key = t

  module Set : Set.S with type elt = t
end

(** Evaluation mode for actions.

    In [Lazy] mode, dependencies are only collected. In [Eager] mode,
    dependencies are build as soon as they are recorded and their facts are
    returned.

    If you want to both evaluate an action builder and build the collected
    dependencies, using [Eager] mode will increase parallelism. If you only want
    to know the set of dependencies, using [Lazy] will avoid unnecessary work. *)
type 'a eval_mode =
  | Lazy : unit eval_mode
  | Eager : Dep.Fact.t eval_mode

type 'a thunk = { f : 'm. 'm eval_mode -> ('a * 'm Dep.Map.t) Memo.Build.t }
[@@unboxed]

val memoize_thunk : string -> 'a thunk -> 'a thunk

type t = private
  { id : Id.t
  ; context : Build_context.t option
  ; targets : Path.Build.Set.t
  ; action : Action.Full.t thunk
  ; mode : Mode.t
  ; info : Info.t
  ; loc : Loc.t
  ; (* Directory where all the targets are produced. *) dir : Path.Build.t
  }

module Set : Set.S with type elt = t

val equal : t -> t -> bool

val hash : t -> int

val to_dyn : t -> Dyn.t

val make :
     ?sandbox:Sandbox_config.t
  -> ?mode:Mode.t
  -> context:Build_context.t option
  -> ?info:Info.t
  -> targets:Path.Build.Set.t
  -> Action.Full.t thunk
  -> t

val set_action : t -> Action.Full.t thunk -> t

val loc : t -> Loc.t

(** [find_source_dir rule] is the closest source directory corresponding to
    rule.dir. Eg. [src/dune] for a rule with dir
    [_build/default/src/dune/.dune.objs]. *)
val find_source_dir : t -> Source_tree.Dir.t Memo.Build.t

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
