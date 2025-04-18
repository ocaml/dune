open Import

module Buildable : sig
  type t =
    { flags : Ordered_set_lang.Unexpanded.t
    ; coq_lang_version : Dune_sexp.Syntax.Version.t
    ; mode : Coq_mode.t option
    ; use_stdlib : bool
    ; plugins : (Loc.t * Lib_name.t) list (** ocaml plugins *)
    ; theories : (Loc.t * Coq_lib_name.t) list (** coq libraries *)
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

  include Stanza.S with type t := t
end

module Theory : sig
  type t =
    { name : Loc.t * Coq_lib_name.t
    ; package : Package.t option
    ; project : Dune_project.t
    ; synopsis : string option
    ; modules : Ordered_set_lang.t
    ; modules_flags : (Coq_module.Name.t * Ordered_set_lang.Unexpanded.t) list option
    ; boot : bool
    ; enabled_if : Blang.t
    ; buildable : Buildable.t
    ; coqdep_flags : Ordered_set_lang.Unexpanded.t
    ; coqdoc_flags : Ordered_set_lang.Unexpanded.t
    ; coqdoc_header : String_with_vars.t option
    ; coqdoc_footer : String_with_vars.t option
    }

  include Stanza.S with type t := t
end

module Coqpp : sig
  type t =
    { modules : Ordered_set_lang.t
    ; loc : Loc.t
    }

  include Stanza.S with type t := t
end

val key : unit Dune_project.Extension.t
