open Stdune

module Select : sig
  module Choice : sig
    type t =
      { required : Lib_name.Set.t
      ; forbidden : Lib_name.Set.t
      ; file : string
      }
  end

  type t =
    { result_fn : string
    ; choices : Choice.t list
    ; loc : Loc.t
    }

  val to_dyn : t -> Dyn.t
end

type t =
  | Direct of (Loc.t * Lib_name.t)
  | Re_export of (Loc.t * Lib_name.t)
  | Select of Select.t

val to_dyn : t -> Dyn.t

val direct : Loc.t * Lib_name.t -> t

val re_export : Loc.t * Lib_name.t -> t

val to_lib_names : t -> Lib_name.t list

val decode : allow_re_export:bool -> t Dune_lang.Decoder.t

val encode : t Dune_lang.Encoder.t

module L : sig
  val field_encode : t list -> name:string -> Dune_lang.Encoder.field
end
