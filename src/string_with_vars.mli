(** String with variables of the form ${...} or $(...)

    Variables cannot contain "${", "$(", ")" or "}". For instance in "$(cat ${x})", only
    "${x}" will be considered a variable, the rest is text. *)

open Import

type t
val t : t Sexp.Of_sexp.t
val sexp_of_t : t -> Sexp.t

val of_string : string -> t

val just_a_var : t -> string option

val vars : t -> String_set.t

val fold : t -> init:'a -> f:('a -> string -> 'a) -> 'a

val expand : t -> f:(string -> string option) -> string
