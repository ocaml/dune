(** Element ordering *)

type t =
  | Lt (** Lesser than *)
  | Eq (** Equal *)
  | Gt (** Greater than *)

val of_int : int -> t
val to_int : t -> int

(** returns the string representation. one of: "<", "=", ">" *)
val to_string : t -> string

val is_eq : t -> bool
val min : ('a -> 'a -> t) -> 'a -> 'a -> 'a
val max : ('a -> 'a -> t) -> 'a -> 'a -> 'a

(** [reverse cmp] takes a comparison function [cmp] and returns a new comparison
    function whose comparisons are the opposite of that of [cmp]. *)
val reverse : ('a -> 'a -> t) -> 'a -> 'a -> t

module O : sig
  (** A convenient operator for efficiently chaining multiple comparisons
      together. For example, you can write

      {v
          let compare { x; y; z } t =
            let open Ordering.O in
            let= () = compare_x x t.x in
            let= () = compare_y y t.y in
            compare_z z t.z
      v}

      or, a bit less compactly but more symmetrically

      {v
          let compare { x; y; z } t =
            let open Ordering.O in
            let= () = compare_x x t.x in
            let= () = compare_y y t.y in
            let= () = compare_z z t.z in
            Eq
      v}

      to chain three comparisons instead of the usual triply nested [match].

      Note that the resulting code can be up to 2x slower than nested [match]ing
      due to extra allocations that we are unable to eliminate (as of Nov 2021),
      so you should use [let=] only where appropriate. *)
  val ( let= ) : t -> (unit -> t) -> t
end
