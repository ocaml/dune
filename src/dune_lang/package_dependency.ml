open! Stdune

type t =
  { name : Package_name.t
  ; constraint_ : Package_constraint.t option
  }

module Well_formed_name = struct
  module T = struct
    include Package_name.Opam_compatible

    let module_ = "Package.Dependency.Name"
    let description = "package dependency"
    let description_of_valid_string = Some description_of_valid_string

    let better_hint s =
      match String.lsplit2 s ~on:'.' with
      | None -> make_valid s
      | Some (before, after) ->
        if Option.is_some (of_string_opt before) && not (String.is_empty after)
        then sprintf "(%s (= %s))" before after
        else make_valid s
    ;;

    let hint_valid = Some better_hint
  end

  module TT = Dune_util.Stringlike.Make (T)

  let decode =
    let open Dune_sexp.Decoder in
    TT.decode >>| T.to_package_name
  ;;
end

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
  <|> let+ name = Well_formed_name.decode in
      { name; constraint_ = None }
;;

let to_dyn { name; constraint_ } =
  let open Dyn in
  record
    [ "name", Package_name.to_dyn name
    ; "constraint_", Dyn.Option (Option.map ~f:Package_constraint.to_dyn constraint_)
    ]
;;

let equal { name; constraint_ } t =
  Package_name.equal name t.name
  && Option.equal Package_constraint.equal constraint_ t.constraint_
;;

module Opam_compatible = struct
  type package_dependency = t

  type t =
    { name : Package_name.Opam_compatible.t
    ; constraint_ : Package_constraint.t option
    }

  let equal { name; constraint_ } t =
    Package_name.Opam_compatible.equal name t.name
    && Option.equal Package_constraint.equal constraint_ t.constraint_
  ;;

  let to_dyn { name; constraint_ } =
    let open Dyn in
    record
      [ "name", Package_name.Opam_compatible.to_dyn name
      ; "constraint_", Dyn.Option (Option.map ~f:Package_constraint.to_dyn constraint_)
      ]
  ;;

  let encode { name; constraint_ } =
    let open Dune_sexp.Encoder in
    match constraint_ with
    | None -> Package_name.Opam_compatible.encode name
    | Some c ->
      pair Package_name.Opam_compatible.encode Package_constraint.encode (name, c)
  ;;

  let decode =
    let open Dune_sexp.Decoder in
    let constrained =
      let+ name = Well_formed_name.T.decode
      and+ expr = Package_constraint.decode in
      { name; constraint_ = Some expr }
    in
    enter constrained
    <|> let+ name = Well_formed_name.T.decode in
        { name; constraint_ = None }
  ;;

  let of_package_dependency ({ name; constraint_ } : package_dependency) =
    { name = Package_name.Opam_compatible.of_package_name_exn name; constraint_ }
  ;;
end
