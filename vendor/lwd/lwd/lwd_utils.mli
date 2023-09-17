type 'a monoid = 'a * ('a -> 'a -> 'a)
(** A monoid, defined by a default element and an associative operation *)

val lift_monoid : 'a monoid -> 'a Lwd.t monoid
(** Use a monoid inside [Lwd] *)

(** {1 List reduction functions}

    All reductions are balanced, relying on operator associativity.

    [fold_left] would compute a chain like:
      [fold f [a; b; c; d] = f a (f b (f c d)]

    [reduce] uses tree-shaped computations like:
      [reduce f [a; b; c; d] = f (f a b) (f c d)]

    The depth of the computation grows in O(log n) where n is the length of the
    input sequence.
*)

val pack : 'a monoid -> 'a Lwd.t list -> 'a Lwd.t
(** Reduce a list of elements in [Lwd] monad *)

val pack_seq : 'a monoid -> 'a Lwd.t Seq.t -> 'a Lwd.t
(** Reduce an (OCaml) [Seq.t] with a monoid *)

val reduce : 'a monoid -> 'a list -> 'a
(** Reduce a list with a monoid **)

val map_reduce : ('a -> 'b) -> 'b monoid -> 'a list -> 'b
(** Map and reduce a list with a monoid **)

(** {1 Other Lwd list functions} *)

val map_l : ('a -> 'b Lwd.t) -> 'a list -> 'b list Lwd.t

val flatten_l : 'a Lwd.t list -> 'a list Lwd.t
(** Commute [Lwd] and [list] *)

(** {1 Miscellaneous functions}

    I don't know where to put these, but they are useful, especially for
    UI-related computations.
*)

val mini : int -> int -> int
(** Minimum of two integers *)

val maxi : int -> int -> int
(** Maximum of two integers *)

val clampi : int -> min:int -> max:int -> int
(** Clamp an integer between two bounds. *)

val minf : float -> float -> float
(** Minimum of two floats *)

val maxf : float -> float -> float
(** Maximum of two floats *)

val clampf : float -> min:float -> max:float -> float
(** Clamp a float between two bounds. *)
