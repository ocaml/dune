(** DSL to define sets that are defined by a membership : 'a -> bool
    function. *)

open! Stdune

type t

val to_dyn : t -> Dyn.t

val decode : t Dune_lang.Decoder.t

val empty : t

(** Always return [true] *)
val true_ : t

(** Always return [false] *)
val false_ : t

val exec : t -> standard:t -> string -> bool
val filter : t -> standard:t -> string list -> string list

val of_glob : Glob.t -> t

val of_pred : (string -> bool) -> t

val compl : t -> t

val union : t list -> t

val diff : t -> t -> t

val of_string_set : String.Set.t -> t
