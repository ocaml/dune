open! Stdune

type t =
  { name : Package_name.t
  ; constraint_ : Package_constraint.t option
  }

module Well_formed_name = struct
  module T = struct
    include Package_name.Opam_compatible

    let module_ = "Package_dependency.Name"
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

(* Check for common typos in package dependency constraints *)
let check_for_version_typo { name; constraint_ } =
  let open Package_constraint in
  match constraint_ with
  | Some (Uop (Relop.Eq, Value.String_literal "version")) ->
    Some
      ( sprintf
          "Possible typo in package dependency for %s: '(= version)' might be a mistake."
          (Package_name.to_string name)
      , "Did you mean to use the `:version` variable instead? Use: (depends (bar :version))" )
  | Some (Bop (Relop.Eq, Value.String_literal "version", _)) ->
    Some
      ( sprintf
          "Possible typo in package dependency for %s: '(= version)' might be a mistake."
          (Package_name.to_string name)
      , "Did you mean to use the `:version` variable instead? Use: (depends (bar :version))" )
  | Some (Bvar var) when String.equal (Package_variable_name.to_string var) "with_test" ->
    Some
      ( sprintf
          "Possible typo in package dependency for %s: ':with_test' might be a mistake."
          (Package_name.to_string name)
      , "Did you mean to use ':with-test' instead? Use: (depends (bar :with-test))" )
  | _ -> None
;;

let decode =
  let open Dune_sexp.Decoder in
  let constrained =
    let+ name = Package_name.decode
    and+ expr = Package_constraint.decode in
    let result = { name; constraint_ = Some expr } in
    (* Check for typos and emit warnings *)
    (match check_for_version_typo result with
     | Some (msg, suggestion) ->
       User_warning.emit [Pp.text msg; Pp.text suggestion]
     | None -> ());
    result
  in
  enter constrained
  <|> let+ name = Well_formed_name.decode in
      { name; constraint_ = None }
;;

let to_dyn { name; constraint_ } =
  let open Dyn in
  record
    [ "name", Package_name.to_dyn name
    ; "constr", Dyn.Option (Option.map ~f:Package_constraint.to_dyn constraint_)
    ]
;;

let equal { name; constraint_ } t =
  Package_name.equal name t.name
  && Option.equal Package_constraint.equal constraint_ t.constraint_
;;