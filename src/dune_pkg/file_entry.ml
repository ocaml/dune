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

let source_repr =
  Repr.variant
    "file-entry-source"
    [ Repr.case "Path" Path.repr ~proj:(function
        | Path path -> Some path
        | Content _ -> None)
    ; Repr.case "Content" Repr.string ~proj:(function
        | Content content -> Some content
        | Path _ -> None)
    ]
;;

type t =
  { original : source
  ; local_file : Path.Local.t
  }

let equal { original; local_file } t =
  source_equal original t.original && Path.Local.equal local_file t.local_file
;;

let repr =
  Repr.record
    "file-entry"
    [ Repr.field "original" source_repr ~get:(fun t -> t.original)
    ; Repr.field "local_file" Path.Local.repr ~get:(fun t -> t.local_file)
    ]
;;

let to_dyn = Repr.to_dyn repr
