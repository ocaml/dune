open Stdune

type source =
  | Path of Path.t
  | Content of string

let source_equal a b =
  match a, b with
  | Path a, Path b -> Path.equal a b
  | Content a, Content b -> String.equal a b
  | Path _, Content _ | Content _, Path _ -> false
;;

let dyn_of_source = function
  | Path path -> Dyn.variant "Path" [ Path.to_dyn path ]
  | Content content -> Dyn.variant "Content" [ Dyn.string content ]
;;

type t =
  { original : source
  ; local_file : Path.Local.t
  }

let equal { original; local_file } t =
  source_equal original t.original && Path.Local.equal local_file t.local_file
;;

let to_dyn { original; local_file } =
  Dyn.record
    [ "original", dyn_of_source original; "local_file", Path.Local.to_dyn local_file ]
;;
