(** A [File_selector.t] is a predicate evaluated on a set of file names in a
    specified directory. *)

open Import

type t

val dir : t -> Path.t
val only_generated_files : t -> bool
val of_glob : dir:Path.t -> Glob.t -> t
val predicate : t -> Predicate_lang.Glob.t

val of_predicate_lang
  :  dir:Path.t
  -> ?only_generated_files:bool
  -> Predicate_lang.Glob.t
  -> t

val equal : t -> t -> bool
val hash : t -> int
val compare : t -> t -> Ordering.t

(** [to_dyn] is used as a marshallable representation of [t] (to compute
    digests), so it must be injective *)
val to_dyn : t -> Dyn.t

val test : t -> Path.t -> bool
val test_basename : t -> basename:string -> bool
val digest : t -> Digest.t
