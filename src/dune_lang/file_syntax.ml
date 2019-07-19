open Stdune

type t = Jbuild | Dune

let equal = (=)

let hash = Hashtbl.hash

let of_basename = function
  | "jbuild" -> Some Jbuild
  | "dune" -> Some Dune
  | _ -> None

let to_dyn =
  let open Dyn.Encoder in
  function
  | Jbuild -> constr "Jbuild" []
  | Dune -> constr "Dune" []
