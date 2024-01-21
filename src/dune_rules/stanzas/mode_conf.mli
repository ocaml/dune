open Import

type t =
  | Byte
  | Native
  | Best (** [Native] if available and [Byte] if not *)

val decode : t Dune_lang.Decoder.t
val compare : t -> t -> Ordering.t
val to_dyn : t -> Dyn.t
val encode : t Dune_lang.Encoder.t

module Kind : sig
  type t =
    | Inherited
    | Requested of Loc.t
end

module Map : sig
  type nonrec 'a t =
    { byte : 'a
    ; native : 'a
    ; best : 'a
    }
end

type mode_conf := t

module Set : sig
  type nonrec t = Kind.t option Map.t

  val of_list : (mode_conf * Kind.t) list -> t
  val decode : t Dune_lang.Decoder.t

  module Details : sig
    type t = Kind.t option
  end

  val eval_detailed : t -> has_native:bool -> Details.t Mode.Dict.t
  val eval : t -> has_native:bool -> Mode.Dict.Set.t
end

module Lib : sig
  type t =
    | Ocaml of mode_conf
    | Melange

  val to_dyn : t -> Dyn.t

  module Map : sig
    type nonrec 'a t =
      { ocaml : 'a Map.t
      ; melange : 'a
      }
  end

  module Set : sig
    type mode_conf := t
    type nonrec t = Kind.t option Map.t

    val of_list : (mode_conf * Kind.t) list -> t
    val decode : t Dune_lang.Decoder.t

    module Details : sig
      type t = Kind.t option
    end

    val default : Loc.t -> t
    val eval_detailed : t -> has_native:bool -> Details.t Lib_mode.Map.t
    val eval : t -> has_native:bool -> Lib_mode.Map.Set.t
    val decode_osl : stanza_loc:Loc.t -> Dune_project.t -> t Dune_lang.Decoder.t
  end
end
