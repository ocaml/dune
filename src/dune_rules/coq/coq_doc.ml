open Import
open Memo.O

let coqdoc_directory ~mode ~obj_dir ~name =
  Path.Build.relative
    obj_dir
    (Coq_lib_name.to_string name
     ^
     match mode with
     | `Html -> ".html"
     | `Latex -> ".tex")
;;

let coqdoc_directory_targets ~dir:obj_dir (theory : Coq_stanza.Theory.t) =
  let+ (_ : Coq_lib.DB.t) =
    (* We force the creation of the coq_lib db here so that errors there can
       appear before any errors to do with directory targets from coqdoc. *)
    let* scope = Scope.DB.find_by_dir obj_dir in
    Scope.coq_libs scope
  in
  let loc = theory.buildable.loc in
  let name = snd theory.name in
  Path.Build.Map.of_list_exn
    [ coqdoc_directory ~mode:`Html ~obj_dir ~name, loc
    ; coqdoc_directory ~mode:`Latex ~obj_dir ~name, loc
    ]
;;
