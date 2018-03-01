(** Merlin rules *)

type t

val make
  :  ?requires:(unit, Lib.t list) Build.t
  -> ?flags:(unit, string list) Build.t
  -> ?preprocess:Jbuild.Preprocess.t
  -> ?libname:string
  -> ?source_dirs: Path.Set.t
  -> ?objs_dirs:Path.Set.t
  -> unit
  -> t

val add_source_dir : t -> Path.t -> t

val merge_all : t list -> t option

(** Add rules for generating the .merlin in a directory *)
val add_rules
  : Super_context.t
  -> dir:Path.t
  -> scope:Scope.t
  -> t
  -> unit
