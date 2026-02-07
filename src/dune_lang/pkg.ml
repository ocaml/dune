open Import

let syntax =
  Syntax.create
    ~experimental:true
    ~name:(Syntax.Name.parse "package")
    ~desc:"the package management language"
    [ (0, 1), `Since (0, 0) ]
;;
