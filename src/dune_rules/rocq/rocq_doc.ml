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
open Memo.O

let rocqdoc_directory ~mode ~obj_dir ~name =
  Path.Build.relative
    obj_dir
    (Rocq_lib_name.to_string name
     ^
     match mode with
     | `Html -> ".html"
     | `Latex -> ".tex")
;;

let rocqdoc_directory_targets ~dir:obj_dir (theory : Rocq_stanza.Theory.t) =
  let+ (_ : Rocq_lib.DB.t) =
    (* We force the creation of the rocq_lib db here so that errors there can
       appear before any errors to do with directory targets from rocqdoc. *)
    let* scope = Scope.DB.find_by_dir obj_dir in
    Scope.rocq_libs scope
  in
  let loc = theory.buildable.loc in
  let name = snd theory.name in
  Path.Build.Map.of_list_exn
    [ rocqdoc_directory ~mode:`Html ~obj_dir ~name, loc
    ; rocqdoc_directory ~mode:`Latex ~obj_dir ~name, loc
    ]
;;
