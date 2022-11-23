type t =
  | Public
  | Private

include Dune_lang.Conv.S with type t := t

val is_public : t -> bool

val is_private : t -> bool

val to_dyn : t -> Dyn.t

module Map : sig
  type visibility := t

  type 'a t =
    { public : 'a
    ; private_ : 'a
    }

  val make_both : 'a -> 'a t

  val find : 'a t -> visibility -> 'a
end
