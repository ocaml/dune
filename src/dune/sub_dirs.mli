open Stdune

module Status : sig
  type t =
    | Data_only
    | Normal
    | Vendored

  val to_dyn : t -> Dyn.t

  module Or_ignored : sig
    type nonrec t =
      | Ignored
      | Status of t
  end

  module Map : sig
    type status

    type 'a t =
      { data_only : 'a
      ; vendored : 'a
      ; normal : 'a
      }

    val find : 'a t -> status -> 'a

    val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t
  end
  with type status := t

  module Set : sig
    type t = bool Map.t

    val all : t

    val normal_only : t
  end
end

val default : Predicate_lang.t Status.Map.t

type status_map

val eval :
     Predicate_lang.t Status.Map.t
  -> dirs:string list
  -> status_map

val status : status_map -> dir:string -> Status.Or_ignored.t

val decode :
  (Predicate_lang.t Status.Map.t * Dune_lang.Ast.t list) Dune_lang.Decoder.t
