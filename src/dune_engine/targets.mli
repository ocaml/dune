open! Stdune
open! Import

(** A set of targets of a build rule.

    A rule can produce a set of files whose names are known upfront, as well as
    a set of "opaque" directories whose contents is initially unknown. *)
type t

(** The empty set of targets. Note that rules are not allowed to have the empty
    set of targets, but it is convenient to construct [t] by aggregating several
    sources of information, for some of which it's OK to be empty. *)
val empty : t

val is_empty : t -> bool

(** Combine the sets of file and directory targets. *)
val combine : t -> t -> t

module File : sig
  (** A single file target. *)
  val create : Path.Build.t -> t
end

module Files : sig
  (** A set of file targets. *)
  val create : Path.Build.Set.t -> t
end

(** A set of file and directory targets. *)
val create : files:Path.Build.Set.t -> dirs:Path.Build.Set.t -> t

val files : t -> Path.Build.Set.t

val dirs : t -> Path.Build.Set.t

module Validation_result : sig
  type t =
    | Valid of { parent_dir : Path.Build.t }
    | No_targets
    | Inconsistent_parent_dir
    | File_and_directory_target_with_the_same_name of Path.Build.t
end

(** Ensure that the set of targets is well-formed. *)
val validate : t -> Validation_result.t

(** The "head" target if [t] is non-empty. If [t] contains at least one file,
    then it's the lexicographically first target file. Otherwise, it's the
    lexicographically first target directory. *)
val head : t -> Path.Build.t option

(** Like [head] but raises a code error if the set of targets is empty. *)
val head_exn : t -> Path.Build.t

val partition_map :
     t
  -> file:(Path.Build.t -> 'a)
  -> dir:(Path.Build.t -> 'b)
  -> 'a list * 'b list

val iter :
  t -> file:(Path.Build.t -> unit) -> dir:(Path.Build.t -> unit) -> unit

val map : t -> f:(files:Path.Build.Set.t -> dirs:Path.Build.Set.t -> 'a) -> 'a

(** File targets are traversed before directory targets. *)
val fold :
     t
  -> init:'a
  -> file:(Path.Build.t -> 'a -> 'a)
  -> dir:(Path.Build.t -> 'a -> 'a)
  -> 'a

val to_dyn : t -> Dyn.t

val pp : t -> _ Pp.t
