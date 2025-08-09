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

(* This code lives in its own module so that it's usable in [Dir_status]
   without creating dependency cycles *)

val rocqdoc_directory
  :  mode:[< `Html | `Latex ]
  -> obj_dir:Path.Build.t
  -> name:Rocq_lib_name.t
  -> Path.Build.t

val rocqdoc_directory_targets
  :  dir:Path.Build.t
  -> Rocq_stanza.Theory.t
  -> Loc.t Path.Build.Map.t Memo.t
