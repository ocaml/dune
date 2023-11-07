open! Stdune

type t =
  { name : Package_name.t
  ; constraint_ : Package_constraint.t option
  }

let encode { name; constraint_ } =
  let open Dune_sexp.Encoder in
  match constraint_ with
  | None -> Package_name.encode name
  | Some c -> pair Package_name.encode Package_constraint.encode (name, c)
;;

let decode =
  let open Dune_sexp.Decoder in
  let constrained =
    let+ name = Package_name.decode
    and+ expr = Package_constraint.decode in
    { name; constraint_ = Some expr }
  in
  enter constrained
  <|> let+ name = Package_name.decode in
      { name; constraint_ = None }
;;

let to_dyn { name; constraint_ } =
  let open Dyn in
  record
    [ "name", Package_name.to_dyn name
    ; "constr", Dyn.Option (Option.map ~f:Package_constraint.to_dyn constraint_)
    ]
;;
