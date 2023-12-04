(** How to sandbox actions *)

(** This module describes the method used to sandbox actions. Choices include:

    - not sandboxing
    - sandboxing by symlinking dependencies
    - sandboxing by copying dependencies
    - sandboxing by hardlinking dependencies
    - sandboxing by copying dependencies, detecting changes and patching back
      the source tree

    In the last mode, Dune applies all the changes that happened in the sandbox
    to the source tree. This includes:

    - applying changes to source files that were dependencies
    - deleting source files that were dependencies and were deleted in the
      sandbox
    - promoting all targets
    - promoting all files that were created and not declared as dependencies or
      targets

    This is a dirty setting, but it is necessary to port projects to Dune that
    don't use a separate directory and have rules that go and create/modify
    random files. *)

open Import

type some =
  | Symlink
  | Copy
  | Hardlink
  | Patch_back_source_tree

type t = some option

val compare : t -> t -> Ordering.t
val equal : t -> t -> bool

module Dict : sig
  type key = t

  type 'a t =
    { none : 'a
    ; symlink : 'a
    ; copy : 'a
    ; hardlink : 'a
    ; patch_back_source_tree : 'a
    }

  val compare : ('a -> 'a -> Ordering.t) -> 'a t -> 'a t -> Ordering.t
  val of_func : (key -> 'a) -> 'a t
  val get : 'a t -> key -> 'a
end

module Set : sig
  type key := t
  type t

  val singleton : key -> t

  (** For rules with (mode patch-back-source-tree). *)
  val patch_back_source_tree_only : t

  val is_patch_back_source_tree_only : t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> Ordering.t
  val of_func : (key -> bool) -> t
  val mem : t -> key -> bool
  val inter : t -> t -> t
  val to_dyn : t -> Dyn.t
end

(** We exclude [Some Patch_back_source_tree] because selecting this mode
    globally via the command line or the config file seems like a terrible
    choice. Also, we want to get rid of this mode eventually. *)
val all_except_patch_back_source_tree : t list

val all : t list
val none : t
val symlink : t
val copy : t
val hardlink : t
val decode : t Dune_sexp.Decoder.t
val to_string : t -> string
val to_dyn : t -> Dyn.t
