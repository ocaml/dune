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

let to_dyn =
  let open Dyn in
  function
  | Directory d -> variant "Directory" [ Path.to_dyn d ]
  | Repo r -> variant "Repo" [ opaque r ]
;;
