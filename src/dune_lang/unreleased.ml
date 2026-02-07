open Import

let syntax =
  Syntax.create
    ~name:(Syntax.Name.parse "unreleased")
    ~desc:"a collection of dune features that have yet to be released"
    ~experimental:true
    [ (0, 1), `Since (3, 22) ]
;;

let since () = Syntax.since syntax (0, 1)
