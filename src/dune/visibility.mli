open! Stdune

type t =
  | Public
  | Private

include Dune_lang.Conv.S with type t := t

val is_public : t -> bool

val is_private : t -> bool

val to_dyn : t -> Dyn.t

module Map : sig
  type 'a t =
    { public : 'a
    ; private_ : 'a
    }

  type visibility

  val make_both : 'a -> 'a t

  val find : 'a t -> visibility -> 'a
end
with type visibility := t
