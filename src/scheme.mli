open! Stdune

(** [Scheme] is a collection of rules for one or multiple directories. *)

open Scheme_intf

module Make(Directory_rules : sig
    type t
    val empty : t
    val union : t -> t -> t
  end) : S with type dir_rules := Directory_rules.t

include S with type dir_rules := Rules.rule

module Gen : sig
  module For_tests : sig
    (* calls [print] every time any code embedded in the [Scheme] runs, be it
       a [Thunk] constructor or an [Approximation] function.

       The argument of [print] identifies which thunk got run (the path to that
       thunk within the [Scheme.t] value).  *)
    val instrument : print:(string -> unit) -> 'a Gen.t -> 'a Gen.t
  end
end
