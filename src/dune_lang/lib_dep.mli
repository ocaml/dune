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

val equal : t -> t -> bool
val to_dyn : t -> Dyn.t
val direct : Loc.t * Lib_name.t -> t
val re_export : Loc.t * Lib_name.t -> t
val decode : allow_re_export:bool -> t Dune_sexp.Decoder.t

module L : sig
  type nonrec t = t list

  val field_encode : t -> name:string -> Dune_sexp.Encoder.field

  val decode
    :  allow_re_export:bool
    -> (t, Dune_sexp.Decoder.values) Dune_sexp.Decoder.parser

  val of_pps : Lib_name.t list -> t
end
