open Import

let syntax =
  Syntax.create
    ~name:"oxcaml"
    ~desc:"experimental support for OxCaml"
    ~experimental:true
    [ (0, 1), `Since (3, 20) ]
;;

(* TODO @maiste: move it to somewhere else. *)
(* let extension = *)
(*   Dune_project.Extension.register syntax (Dune_lang.Decoder.return ((), [])) Dyn.unit *)
(* ;; *)
