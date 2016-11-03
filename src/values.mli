(** Values associated to s-expression files *)

open! Import

type 'a t =
  | []     : unit t
  | ( :: ) : 'a * 'b t -> ('a -> 'b) t

module Spec : sig
  type 'a t =
    | []     : unit t
    | ( :: ) : (string (* Path *) * 'a Kind.t) * 'b t -> ('a -> 'b) t

  val filenames : 'a t -> string list
end
