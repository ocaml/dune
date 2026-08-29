(** A possibly infinite set of subdirectories *)

open Import

type t

val all : t
val of_set : Filename.Set.t -> t
val to_dir_set : t -> Path.Unspecified.w Dir_set.t
val of_dir_set : 'a Dir_set.t -> t
val of_list : Filename.t list -> t
val empty : t
val mem : t -> Filename.t -> bool
val union : t -> t -> t
