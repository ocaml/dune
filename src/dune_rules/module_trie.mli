(** This module defines a prefix tree that stores a map of module names at every
    non-leaf node. Most notably, it's used to implement `(include_subdirs
    qualified)` where the directory name qualifies the namespace for its
    descendant modules in the file system. *)

type 'a t = 'a node Module_name.Map.t

and 'a node =
  | Leaf of 'a
  | Map of 'a t

type key = Module_name.Path.t

val empty : 'a t

val map : 'a t -> f:('a -> 'b) -> 'b t

val mapi : 'a t -> f:(key -> 'a -> 'b) -> 'b t

val of_map : 'a Module_name.Map.t -> 'a t

val find : 'a t -> key -> 'a option

val set : 'a t -> key -> 'a -> 'a t

val set_map : 'a t -> key -> 'a Module_name.Map.t -> 'a t

val remove : 'a t -> key -> 'a t

val mem : 'a t -> key -> bool

val fold : 'a t -> init:'acc -> f:('a -> 'acc -> 'acc) -> 'acc

val foldi : 'a t -> init:'acc -> f:(key -> 'a -> 'acc -> 'acc) -> 'acc

val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t

val to_map : 'a t -> 'a Module_name.Map.t

val singleton : key -> 'a -> 'a t

val merge :
  'a t -> 'b t -> f:(key -> 'a option -> 'b option -> 'c option) -> 'c t

val as_singleton : 'a t -> 'a option

val filter_map : 'a t -> f:('a -> 'b option) -> 'b t

val toplevel_only : 'a t -> 'a Module_name.Map.t
