(** Potentially infinite sets of directories *)

open Import

(** Type of potentially infinite sets of directories. Not all sets can be
    represented, only ones that can be efficiently inspected. *)
type 'w t

(** [mem t p] is [true] if and only if [p] is in [t] *)
val mem : 'w t -> 'w Path.Local_gen.t -> bool

(** [here t] is the same as [mem t Path.Build.root] but more efficient. *)
val here : 'w t -> bool

(** The empty set *)
val empty : 'w t

(** The set of all possible directories *)
val universal : 'w t

(** [trivial b] is such that for all path [p]:

    {[
      mem (trivial b) p = b
    ]}

    i.e. [trivial false] is [empty] and [trivial true] is [universal]. *)
val trivial : bool -> 'w t

val is_empty : 'w t -> bool
val is_universal : 'w t -> bool

(** [descend t comp] is the set [t'] such that for all path [p], [p] is in [t']
    iff [comp/p] is in [t]. [comp] must be a path component, i.e. without
    directory separator characters. *)
val descend : 'w t -> Filename.t -> Path.Local.w t

(** [exceptions t] is the set of all bindings of the form [(comp, t')] such
    that:

    - [t' = descend t comp] - [t' <> trivial (default t)]

    Sets of directories for which [exceptions t] is not finite cannot be
    represented by this module. *)
val exceptions : 'w t -> Path.Local.w t Filename.Map.t

(** Default membership value for paths that are neither empty nor part of the
    exceptions. I.e. for all non-empty path [p] whose first component is not in
    [exceptions t], [mem t p = default t]. *)
val default : 'w t -> bool

val create
  :  default:bool
  -> here:bool
  -> exceptions:Path.Unspecified.w t Filename.Map.t
  -> Path.Unspecified.w t

(** [singleton p] is the set containing only [p] *)
val singleton : 'w Path.Local_gen.t -> 'w t

(** [subtree p] is the set of all directories that are descendant of [p]. *)
val subtree : 'w Path.Local_gen.t -> 'w t

val is_subset : 'w t -> of_:'w t -> bool
val union : 'w t -> 'w t -> 'w t
val union_all : 'w t list -> 'w t
val inter : 'w t -> 'w t -> 'w t
val inter_all : 'w t list -> 'w t
val diff : 'w t -> 'w t -> 'w t
val negate : 'w t -> 'w t
val to_dyn : 'w t -> Dyn.t
val forget_root : 'w t -> Path.Unspecified.w t

type toplevel_subdirs =
  | Infinite
  | Finite of Filename.Set.t

val toplevel_subdirs : 'w t -> toplevel_subdirs
val of_list : 'w Path.Local_gen.t list -> 'w t
val just_the_root : 'w t
