(** Module visibility *)

type t =
  | Public
  | Private

include Dune_sexp.Conv.S with type t := t

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
