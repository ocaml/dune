(***********************************************)
(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2019-2024                         *)
(* (c) Emilio J. Gallego Arias 2024-2025       *)
(* (c) CNRS 2025                               *)
(***********************************************)
(* Written by: Ali Caglayan                    *)
(* Written by: Emilio Jesús Gallego Arias      *)
(* Written by: Rudi Grinberg                   *)
(* Written by: Rodolphe Lepigre                *)
(***********************************************)

open Import

module Buildable : sig
  type t =
    { flags : Ordered_set_lang.Unexpanded.t
    ; coq_lang_version : Dune_sexp.Syntax.Version.t
    ; mode : Rocq_mode.t option
    ; use_stdlib : bool
    ; plugins : (Loc.t * Lib_name.t) list (** ocaml plugins *)
    ; theories : (Loc.t * Rocq_lib_name.t) list (** rocq libraries *)
    ; loc : Loc.t
    }
end

module Extraction : sig
  type t =
    { extracted_modules : string list
    ; prelude : Loc.t * Rocq_module.Name.t
    ; buildable : Buildable.t
    }

  val ml_target_fnames : t -> string list

  include Stanza.S with type t := t
end

module Theory : sig
  type t =
    { name : Loc.t * Rocq_lib_name.t
    ; package : Package.t option
    ; project : Dune_project.t
    ; synopsis : string option
    ; modules : Ordered_set_lang.t
    ; modules_flags : (Rocq_module.Name.t * Ordered_set_lang.Unexpanded.t) list option
    ; boot : bool
    ; generate_project_file : Loc.t * bool
    ; enabled_if : Blang.t
    ; buildable : Buildable.t
    ; rocqdep_flags : Ordered_set_lang.Unexpanded.t
    ; rocqdoc_flags : Ordered_set_lang.Unexpanded.t
    ; rocqdoc_header : String_with_vars.t option
    ; rocqdoc_footer : String_with_vars.t option
    }

  include Stanza.S with type t := t
end

module Rocqpp : sig
  type t =
    { modules : Ordered_set_lang.t
    ; loc : Loc.t
    }

  include Stanza.S with type t := t
end

val key : unit Dune_project.Extension.t
