open Import

type t =
  { meta_file : Path.t
  ; name : Lib_name.t
  ; dir : Path.t
  ; vars : Vars.t
  }

val version : t -> string option
val description : t -> string option
val jsoo_runtime : t -> Path.t list
val requires : t -> Lib_name.t list
val ppx_runtime_deps : t -> Lib_name.t list
val kind : t -> [> `Normal | `Ppx_deriver | `Ppx_rewriter ]
val archives : t -> Path.t list Mode.Dict.t
val plugins : t -> Path.t list Mode.Dict.t
val exists : t -> is_builtin:bool -> bool Memo.t

val candidates
  :  dir:Path.Outside_build_dir.t
  -> Package.Name.t
  -> Path.Outside_build_dir.t list

val load_meta
  :  file:Path.Outside_build_dir.t
  -> Package.Name.t option
  -> (Meta.Simplified.t, [> `Does_not_exist ]) result Memo.t
