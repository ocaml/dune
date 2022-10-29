open Import

(** Stanza to produce JavaScript targets from Melange libraries *)
module Emit : sig
  type t =
    { loc : Loc.t
    ; target : string
    ; module_system : Melange.Module_system.t
    ; libraries : Lib_dep.t list
    }

  val decode : t Dune_lang.Decoder.t
end
