open Import

(** Stanza to produce JavaScript targets from Melange libraries *)
module Emit : sig
  type t =
    { loc : Loc.t
    ; target : string
    ; spec : Melange.Spec.t
    ; libraries : Lib_dep.t list
    }

  val decode : t Dune_lang.Decoder.t
end
