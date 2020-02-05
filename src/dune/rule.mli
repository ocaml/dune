(** Representation of rules *)

open! Stdune
open! Import

module Info : sig
  type t =
    | From_dune_file of Loc.t
    | Internal
    | Source_file_copy

  val of_loc_opt : Loc.t option -> t

  val loc : t -> Loc.t option
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
    | Standard  (** Only use this rule if the source files don't exist. *)
    | Fallback  (** Silently promote the targets to the source tree. *)
    | Promote of Promote.t
        (** Just ignore the source files entirely. This is for cases where the
            targets are promoted only in a specific context, such as for
            .install files. *)
    | Ignore_source_files
end

module Id : sig
  type t

  val compare : t -> t -> Ordering.t

  module Map : Map.S with type key = t

  module Top_closure : sig
    val top_closure :
         key:('a -> t)
      -> deps:('a -> 'a list)
      -> 'a list
      -> ('a list, 'a list) Stdune.result
  end
end

type t = private
  { id : Id.t
  ; context : Context.t option
  ; env : Env.t option
  ; action : Action.t Build.With_targets.t
  ; mode : Mode.t
  ; locks : Path.t list
  ; info : Info.t
  ; (* Directory where all the targets are produced. *) dir : Path.Build.t
  }

module Set : Set.S with type elt = t

val equal : t -> t -> bool

val hash : t -> int

val to_dyn : t -> Dyn.t

val make :
     ?sandbox:Sandbox_config.t
  -> ?mode:Mode.t
  -> context:Context.t option
  -> env:Env.t option
  -> ?locks:Path.t list
  -> ?info:Info.t
  -> ?dir:Path.Build.t
  -> Action.t Build.With_targets.t
  -> t

val with_prefix : t -> build:unit Build.t -> t
