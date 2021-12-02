(** A possibly infinite set of subdirectories *)

open! Stdune

type t =
  | All
  | These of String.Set.t

val to_dir_set : t -> Path.Unspecified.w Dir_set.t

val of_dir_set : 'a Dir_set.t -> t

val of_list : string list -> t

val empty : t

val is_empty : t -> bool

val mem : t -> string -> bool

val union : t -> t -> t

val inter_set : t -> String.Set.t -> String.Set.t

val union_all : t list -> t
