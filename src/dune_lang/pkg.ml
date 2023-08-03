let syntax =
  Dune_sexp.Syntax.create
    ~experimental:true
    ~name:"package"
    ~desc:"the package management language"
    [ (0, 1), `Since (0, 0) ]
;;
