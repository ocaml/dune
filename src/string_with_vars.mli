(** String with variables of the form ${...} or $(...)

    Variables cannot contain "${", "$(", ")" or "}". For instance in "$(cat ${x})", only
    "${x}" will be considered a variable, the rest is text. *)

open Import

type t
val t : Sexp.t -> t
val sexp_of_t : t -> Sexp.t

val of_string : string -> t

val vars : t -> String_set.t

val fold : t -> init:'a -> f:('a -> string -> 'a) -> 'a

val expand : t -> f:(string -> string option) -> string

module type Container = sig
  type 'a t
  val t : (Sexp.t -> 'a) -> Sexp.t -> 'a t
  val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t

  val map : 'a t -> f:('a -> 'b) -> 'b t
  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
end

module Lift(M : Container) : sig
  type nonrec t = t M.t
  val t : Sexp.t -> t

  val sexp_of_t : t -> Sexp.t

  val fold : t -> init:'a -> f:('a -> string -> 'a) -> 'a

  val expand : t -> f:(string -> string option) -> string M.t
end
