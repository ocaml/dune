(** String with variables of the form ${...} or $(...)

    Variables cannot contain "${", "$(", ")" or "}". For instance in "$(cat ${x})", only
    "${x}" will be considered a variable, the rest is text. *)

open Import

type t
val t : t Sexp.Of_sexp.t
val sexp_of_t : t -> Sexp.t

val of_string : string -> t

val vars : t -> String_set.t

val fold : t -> init:'a -> f:('a -> string -> 'a) -> 'a

val expand : t -> f:(string -> string option) -> string

module type Container = sig
  type 'a t
  val t : 'a Sexp.Of_sexp.t -> 'a t Sexp.Of_sexp.t
  val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t

  type context
  val expand : context -> 'a t -> f:(context -> 'a -> string) -> string t
  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
end

module Lift(M : Container) : sig
  type nonrec t = t M.t
  val t : t Sexp.Of_sexp.t

  val sexp_of_t : t -> Sexp.t

  val fold : t -> init:'a -> f:('a -> string -> 'a) -> 'a

  val expand
    :  M.context
    -> t
    -> f:(M.context -> string -> string option)
    -> string M.t
end
