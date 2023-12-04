(** A set of elements that can be represented by a single word *)

module Make (Element : sig
    type t

    (** [to_int t] must return a unique number for every [t] between [0] and
        [Sys.int_size - 1] (inclusive) *)
    val to_int : t -> int

    (** [all] contains all possible set elements *)
    val all : t list

    val to_dyn : t -> Dyn.t
  end) : sig
  type t [@@immediate]

  val empty : t
  val singleton : Element.t -> t
  val add : t -> Element.t -> t
  val inter : t -> t -> t
  val union : t -> t -> t
  val mem : t -> Element.t -> bool
  val to_dyn : t -> Dyn.t
  val compare : t -> t -> Ordering.t
  val equal : t -> t -> bool
  val of_func : (Element.t -> bool) -> t
end
