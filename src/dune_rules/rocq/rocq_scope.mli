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
  -> db_by_project_output_root:(Loaded_project.t * Lib.DB.t) Path.Build.Map.t
  -> (Loaded_project.t * Path.Build.t * Rocq_stanza.Theory.t) list
  -> t

val find : t -> project:Loaded_project.t -> Rocq_lib.DB.t Memo.t
