open Import

let latest_version = 0, 1

let syntax =
  Syntax.create
    ~name:"oxcaml"
    ~desc:"experimental support for OxCaml"
    ~experimental:true
    [ latest_version, `Since (3, 20) ]
;;

let parameterised_dir = ".parameterised"
