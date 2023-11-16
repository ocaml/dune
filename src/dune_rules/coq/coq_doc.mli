open Import

(* This code lives in its own module so that it's usable in [Dir_status]
   without creating dependency cycles *)

val coqdoc_directory
  :  mode:[< `Html | `Latex ]
  -> obj_dir:Path.Build.t
  -> name:Coq_lib_name.t
  -> Path.Build.t

val coqdoc_directory_targets
  :  dir:Path.Build.t
  -> Coq_stanza.Theory.t
  -> Loc.t Path.Build.Map.t Memo.t
