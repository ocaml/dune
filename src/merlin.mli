(** Merlin rules *)

type t =
  { requires   : (unit, Lib.t list) Build.t
  ; flags      : (unit, string list) Build.t
  ; preprocess : Jbuild.Preprocess.t
  ; libname    : string option
  ; source_dirs: Path.Set.t
  }

val merge_all : t list -> t option

(** Add rules for generating the .merlin in a directory *)
val add_rules
  : Super_context.t
  -> dir:Path.t
  -> scope:Lib_db.Scope.t Lib_db.with_required_by
  -> t
  -> unit

