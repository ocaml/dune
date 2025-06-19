open Import

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
let dependency_constraint_variable_typo_warnings ~loc { name; constraint_ } =
  match Dune_config.Config.(get typo_warnings) with
  | `Enabled ->
    (match constraint_ with
     | Some
         (Package_constraint.Uop
            (Relop.Eq, Package_constraint.Value.String_literal "version")) ->
       let message =
         User_message.make
           ~loc
           [ Pp.textf
               "Possible typo in constraint for dependency %S: '(= version)' might be a \
                mistake."
               (Package_name.to_string name)
           ]
           ~hints:
             [ Pp.textf
                 "Did you mean to use the `:version` variable instead? Example: (depends \
                  (%s (= :version)))"
                 (Package_name.to_string name)
             ]
       in
       Some message
     | Some (Package_constraint.Bvar var)
       when String.equal (Package_variable_name.to_string var) "with_test" ->
       let message =
         User_message.make
           ~loc
           [ Pp.textf
               "Possible typo in constraint for dependency %S: ':with_test' might be a \
                mistake."
               (Package_name.to_string name)
           ]
           ~hints:
             [ Pp.textf
                 "Did you mean to use ':with-test' instead? Example: (depends (%s \
                  :with-test))"
                 (Package_name.to_string name)
             ]
       in
       Some message
     | _ -> None)
  | `Disabled -> None
;;

let decode =
  let open Decoder in
  let constrained =
    let+ loc = loc
    and+ name = Package_name.decode
    and+ expr = Package_constraint.decode in
    let result = { name; constraint_ = Some expr } in
    (match dependency_constraint_variable_typo_warnings ~loc result with
     | Some msg -> User_warning.emit_message msg
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
