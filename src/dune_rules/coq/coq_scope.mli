open Import

type t

val make
  :  Context.t
  -> public_libs:Lib.DB.t
  -> db_by_project_dir:('a * Lib.DB.t) Path.Source.Map.t
  -> projects_by_root:'b Path.Source.Map.t
  -> (Path.Build.t * Coq_stanza.Theory.t) list
  -> t

val find : t -> dir:Path.Source.t -> Coq_lib.DB.t Memo.t
