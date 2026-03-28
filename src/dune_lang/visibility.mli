open Import

(** Module visibility *)

type t =
  | Public
  | Private
  | Excluded

include Conv.S with type t := t

val to_dyn : t -> Dyn.t

module Map : sig
  type visibility := t

  type 'a t =
    { public : 'a
    ; private_ : 'a
    ; excluded : 'a
    }

  val make_both : 'a -> 'a t
  val find : 'a t -> visibility -> 'a
end
