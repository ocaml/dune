open Stdune

type t =
  | Impl
  | Intf

val all : t list

val choose : t -> impl:'a -> intf:'a -> 'a

(** "" or "i" *)
val suffix : t -> string

val to_string : t -> string

val to_dyn : t -> Dyn.t

val cmt_ext : t -> string

module Dict : sig
  type kind := t

  type 'a t =
    { impl : 'a
    ; intf : 'a
    }

  val compare : ('a -> 'a -> Ordering.t) -> 'a t -> 'a t -> Ordering.t

  val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t

  val get : 'a t -> kind -> 'a

  val of_func : (ml_kind:kind -> 'a) -> 'a t

  val make_both : 'a -> 'a t

  val iteri : 'a t -> f:(kind -> 'a -> unit) -> unit

  val make : impl:'a -> intf:'a -> 'a t

  val mapi : 'a t -> f:(kind -> 'a -> 'b) -> 'b t

  val map : 'a t -> f:('a -> 'b) -> 'b t
end
