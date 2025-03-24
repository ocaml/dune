(** Like [Path.Set.t] but tailored for representing sets of file names in the same parent
    directory. Compared to [Path.Set.t], [Filename_set.t] statically enforces an important
    invariant, and can also be processed more efficiently. *)

type t

val equal : t -> t -> bool

(** The directory of the filename set. *)
val dir : t -> Path.t

(** The set of file names, all relative to [dir]. *)
val filenames : t -> Filename.Set.t

val empty : dir:Path.t -> t
val is_empty : t -> bool

(* CR-soon amokhov: Decouple [create] from [filter]. *)
val create : ?filter:(basename:string -> bool) -> dir:Path.t -> Filename.Set.t -> t
val to_list : t -> Path.t list
