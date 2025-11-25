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

type t

val make
  :  Context.t
  -> public_libs:Lib.DB.t
  -> db_by_project_dir:('a * Lib.DB.t) Path.Source.Map.t
  -> projects_by_root:'b Path.Source.Map.t
  -> (Path.Build.t * Rocq_stanza.Theory.t) list
  -> t

val find : t -> dir:Path.Source.t -> Rocq_lib.DB.t Memo.t
