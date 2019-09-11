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
  end
  with type status := t

  module Set : sig
    type t = bool Map.t

    val all : t

    val normal_only : t
  end
end

val default : Predicate_lang.t Status.Map.t

val eval :
     Predicate_lang.t Status.Map.t
  -> dirs:string list
  -> String.Set.t Status.Map.t

val status : String.Set.t Status.Map.t -> dir:string -> Status.Or_ignored.t

val decode :
  (Predicate_lang.t Status.Map.t * Dune_lang.Ast.t list) Dune_lang.Decoder.t
