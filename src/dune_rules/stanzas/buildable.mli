open Import

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
  ; js_of_ocaml : Js_of_ocaml.In_buildable.t
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
