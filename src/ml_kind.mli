open Stdune

type t = Impl | Intf

val all : t list

val pp : t Fmt.t

(** "" or "i" *)
val suffix : t -> string

val to_string : t -> string

val to_dyn : t -> Dyn.t

val flag : t -> _ Command.Args.t
val ppx_driver_flag : t -> _ Command.Args.t

module Dict : sig
  type kind = t

  type 'a t =
    { impl : 'a
    ; intf : 'a
    }

  val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t

  val get : 'a t -> kind -> 'a

  val of_func : (ml_kind:kind -> 'a) -> 'a t

  val make_both : 'a -> 'a t

  val iteri : 'a t -> f:(kind -> 'a -> unit) -> unit

  val make : impl:'a -> intf:'a -> 'a t

  val mapi : 'a t -> f:(kind -> 'a -> 'b) -> 'b t

  val map : 'a t -> f:('a -> 'b) -> 'b t
end with type kind := t
