open Import

let syntax =
  Syntax.create
    ~name:"oxcaml"
    ~desc:"experimental support for OxCaml"
    ~experimental:true
    [ (0, 1), `Since (3, 20) ]
;;

let extension =
  Dune_project.Extension.register syntax (Dune_lang.Decoder.return ((), [])) Dyn.unit
;;
