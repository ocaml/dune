open Import

module Entry : sig
  type t = Module of Module_name.t
end

(** Stanza to produce JavaScript targets from Melange libraries *)
module Emit : sig
  type t =
    { loc : Loc.t
    ; target : string
    ; module_system : Melange.Module_system.t
    ; entries : Entry.t list
    ; libraries : Lib_dep.t list
    ; package : Package.t option
    }

  val decode : t Dune_lang.Decoder.t
end
