open Import

type t =
  { meta_file : Path.t
  ; name : Lib_name.t
  ; dir : Path.t
  ; vars : Vars.t
  }

val meta_fn : Filename.t
val version : t -> string option
val description : t -> string option
val jsoo_runtime : t -> Path.t list
val requires : t -> Lib_name.t list
val ppx_runtime_deps : t -> Lib_name.t list
val kind : t -> Lib_kind.t
val archives : t -> Path.t list Mode.Dict.t
val plugins : t -> Path.t list Mode.Dict.t
val findlib_predicates_set_by_dune : Ocaml.Variant.Set.t
val exists : t -> is_builtin:bool -> bool Memo.t

val candidates
  :  dir:Path.Outside_build_dir.t
  -> Package.Name.t
  -> Path.Outside_build_dir.t list
