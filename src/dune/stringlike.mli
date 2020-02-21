(* CR-someday aalekseyev: make the return type [with type t = private S.t] for
   better safety against accidental injection of unvalidated values *)

(** Create a standard set of functions from a base pair of to/from string
    converters *)
module Make (S : Stringlike_intf.S_base) : Stringlike_intf.S with type t = S.t
