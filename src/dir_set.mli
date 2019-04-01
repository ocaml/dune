open! Stdune

(** Represents a (potentially infinite) set of directories. Not any set can be specified,
    only ones that can be efficiently inspected. *)

type children

type t

(* Total mapping from the child basename to a [t].
   Only a finite number of bindings can be non-trivial.

   The "trivial" ones will be either all [trivial true] or all [trivial false]. *)
module Children : sig
  type set = t
  type t = children

  val default : t -> bool
  val exceptions : t -> set String.Map.t

  val create : default:bool -> exceptions:set String.Map.t -> t
end

val here : t -> bool
val children : t -> Children.t

val empty : t
val universal : t

val is_empty : t -> bool
val is_universal : t -> bool

val mem : t -> Path.Build.t -> bool

val descend : t -> string -> t

val of_subtrees : Path.Build.t list -> t
val of_individual_dirs : Path.Build.t list -> t

type element =
  | One_dir of Path.Build.t
  | Subtree of Path.Build.t

val of_list : element list -> t

val is_subset : t -> of_:t -> bool

val union : t -> t -> t
val intersect : t -> t -> t
val negate : t -> t

val to_sexp : t -> Sexp.t
