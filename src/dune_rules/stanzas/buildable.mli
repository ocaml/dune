open Import
module Ocaml_flags := Dune_lang.Ocaml_flags

type for_ =
  | Executable
  | Library of Wrapped.t option

type t =
  { loc : Loc.t
  ; modules : Stanza_common.Modules_settings.t
  ; empty_module_interface_if_absent : bool
  ; libraries : Lib_dep.t list
  ; foreign_archives : (Loc.t * Foreign.Archive.t) list
  ; extra_objects : Foreign.Objects.t
  ; foreign_stubs : Foreign.Stubs.t list
  ; preprocess : Preprocess.With_instrumentation.t Preprocess.Per_module.t
  ; preprocessor_deps : Dep_conf.t list
  ; lint : Lint.t
  ; flags : Ocaml_flags.Spec.t
  ; js_of_ocaml : Js_of_ocaml.In_buildable.t Js_of_ocaml.Mode.Pair.t
  ; allow_overlapping_dependencies : bool
  ; ctypes : Ctypes_field.t option
  }

(** Check if the buildable has any foreign stubs or archives. *)
val has_foreign : t -> bool

(** Check if the buildable has any c++ foreign stubs. *)
val has_foreign_cxx : t -> bool

val has_mode_dependent_foreign_stubs : t -> bool
val decode : for_ -> t Dune_lang.Decoder.fields_parser
val has_foreign_stubs : t -> bool

(** Parser for the libraries fields *)
val decode_libraries : allow_re_export:bool -> Lib_dep.L.t Dune_lang.Decoder.fields_parser

(** Parser for the preprocesss *)
val decode_preprocess
  : ( Preprocess.With_instrumentation.t Preprocess.Per_module.t * Dep_conf.t list
      , Dune_lang.Decoder.fields )
      Dune_lang.Decoder.parser

(** Parser for the ocaml flags *)
val decode_ocaml_flags : Ocaml_flags.Spec.t Dune_lang.Decoder.fields_parser

(* Parser for the modules field *)
val decode_modules : Stanza_common.Modules_settings.t Dune_lang.Decoder.fields_parser

(* Parser for the lint field *)
val decode_lint
  : Preprocess.Without_instrumentation.t Preprocess.t Module_name.Per_item.t
      Dune_lang.Decoder.fields_parser

(* Parser for allow_overlapping_dependencies *)
val decode_allow_overlapping : bool Dune_lang.Decoder.fields_parser
