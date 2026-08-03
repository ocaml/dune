open Import

type t =
  | Directory of Path.t
  | Repo of Rev_store.At_rev.t

let equal a b =
  match a, b with
  | Directory a, Directory b -> Path.equal a b
  | Repo a, Repo b -> Rev_store.At_rev.equal a b
  | _, _ -> false
;;

let repr =
  Repr.variant
    "source-backend"
    [ Repr.case "Directory" Path.repr ~proj:(function
        | Directory path -> Some path
        | Repo _ -> None)
    ; Repr.case "Repo" (Repr.abstract Dyn.opaque) ~proj:(function
        | Repo repo -> Some repo
        | Directory _ -> None)
    ]
;;

let to_dyn = Repr.to_dyn repr
