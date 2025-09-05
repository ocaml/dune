open Import

let syntax =
  Syntax.create
    ~name:"oxcaml"
    ~desc:"experimental support for OxCaml"
    ~experimental:true
    [ (0, 1), `Since (3, 20) ]
;;
