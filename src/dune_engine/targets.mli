open! Stdune
open! Import

(* CR-someday amokhov: Add directory targets. *)

(** A set of file targets of a build rule. *)
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

module Validation_result : sig
  type t =
    | Valid of { parent_dir : Path.Build.t }
    | No_targets
    | Inconsistent_parent_dir
end

(** Ensure that the set of targets is non-empty and that all targets have the
    same parent dir. *)
val validate : t -> Validation_result.t

(** The "head" target, i.e. the lexicographically first target file if [t] is
    non-empty. *)
val head : t -> Path.Build.t option

(** Like [head] but raises a code error if the set of targets is empty. *)
val head_exn : t -> Path.Build.t

val files : t -> Path.Build.Set.t

val to_list_map : t -> file:(Path.Build.t -> 'a) -> 'a list

val fold : t -> init:'a -> file:(Path.Build.t -> 'a -> 'a) -> 'a

val iter : t -> file:(Path.Build.t -> unit) -> unit

val to_dyn : t -> Dyn.t

val pp : t -> _ Pp.t
