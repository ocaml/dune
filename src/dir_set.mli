(** Potentially infinite sets of directories *)

open! Stdune

(** Type of potentially infinite sets of directories. Not all sets can
    be represented, only ones that can be efficiently inspected. *)
type t

(** [mem t p] is [true] if and only if [p] is in [t] *)
val mem : t -> Path.Build.t -> bool

(** [here t] is the same as [mem t Path.Build.root] but more
    efficient. *)
val here : t -> bool

(** The empty set *)
val empty : t

(** The set of all possible directories *)
val universal : t

(** [trivial b] is such that for all path [p]:

    {[
      mem (trivial b) p = b
    ]}

    i.e. [trivial false] is [empty] and [trivial true] is [universal].
*)
val trivial : bool -> t

val is_empty : t -> bool
val is_universal : t -> bool

(** [descend t comp] is the set [t'] such that for all path [p], [p]
    is in [t'] iff [comp/p] is in [t]. [comp] must be a path component,
    i.e. without directory separator characters. *)
val descend : t -> string -> t

(** [exceptions t] is the set of all bindings of the form [(comp,
    t']] such that:

    - [t' = descend t comp]
    - [t' <> trivial (default t)]

    Sets of directories for which [exceptions t] is not finite cannot be
    represented by this module.
*)
val exceptions : t -> t String.Map.t

(** Default membership value for paths that are neither empty nor part
    of the exceptions. I.e. for all non-empty path [p] whose first
    component is not in [exceptions t], [mem t p = default t]. *)
val default : t -> bool

(** [singleton p] is the set containing only [p] *)
val singleton : Path.Build.t -> t

(** [subtree p] is the set of all directories that are descendant of
    [p]. *)
val subtree : Path.Build.t -> t

val is_subset : t -> of_:t -> bool
val union : t -> t -> t
val union_all : t list -> t
val inter : t -> t -> t
val inter_all : t list -> t
val diff : t -> t -> t
val negate : t -> t

val to_sexp : t -> Sexp.t
