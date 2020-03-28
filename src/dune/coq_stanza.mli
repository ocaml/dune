open Import

module Buildable : sig
  type t =
    { flags : Ordered_set_lang.Unexpanded.t
    ; libraries : (Loc.t * Lib_name.t) list  (** ocaml libraries *)
    ; theories : (Loc.t * Coq_lib_name.t) list  (** coq libraries *)
    ; loc : Loc.t
    }
end

module Extraction : sig
  type t =
    { extracted_modules : string list
    ; prelude : Loc.t * Coq_module.Name.t
    ; buildable : Buildable.t
    }

  val ml_target_fnames : t -> string list

  type Stanza.t += T of t
end

module Theory : sig
  type t =
    { name : Loc.t * Coq_lib_name.t
    ; package : Package.t option
    ; project : Dune_project.t
    ; synopsis : string option
    ; modules : Ordered_set_lang.t
    ; boot : bool
    ; enabled_if : Blang.t
    ; buildable : Buildable.t
    }

  type Stanza.t += T of t
end

module Coqpp : sig
  type t =
    { modules : string list
    ; loc : Loc.t
    }

  type Stanza.t += T of t
end

val key : unit Dune_project.Extension.t
