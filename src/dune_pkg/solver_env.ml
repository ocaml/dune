open Import

type t = Variable_value.t Package_variable_name.Map.t

let empty = Package_variable_name.Map.empty
let equal = Package_variable_name.Map.equal ~equal:Variable_value.equal
let to_dyn = Package_variable_name.Map.to_dyn Variable_value.to_dyn
let is_empty = Package_variable_name.Map.is_empty

let validate t ~loc =
  if Package_variable_name.Map.mem t Package_variable_name.with_test
  then
    User_error.raise
      ?loc
      [ Pp.textf
          "Setting the %S solver variable is not permitted as it would conflict with \
           dune's internal use of %S while solving opam packages."
          Package_variable_name.(to_string with_test)
          Package_variable_name.(to_string with_test)
      ]
;;

let decode =
  let open Decoder in
  let+ loc, bindings =
    located (repeat (pair Package_variable_name.decode Variable_value.decode))
  in
  match Package_variable_name.Map.of_list bindings with
  | Ok t ->
    validate t ~loc:(Some loc);
    t
  | Error (duplicate_key, a, b) ->
    User_error.raise
      ~loc
      [ Pp.textf
          "Duplicate entries for user variable %s (%s, %s)"
          (String.maybe_quoted (Package_variable_name.to_string duplicate_key))
          (String.maybe_quoted (Variable_value.to_string a))
          (String.maybe_quoted (Variable_value.to_string b))
      ]
;;

let set t variable_name variable_value =
  let t = Package_variable_name.Map.set t variable_name variable_value in
  validate t ~loc:None;
  t
;;

let get = Package_variable_name.Map.find
let extend a b = Package_variable_name.Map.superpose b a

let with_defaults =
  [ ( Package_variable_name.opam_version
    , OpamVersion.to_string OpamVersion.current |> Variable_value.string )
  ; Package_variable_name.with_doc, Variable_value.false_
  ; Package_variable_name.with_dev_setup, Variable_value.false_
  ]
  |> List.fold_left ~init:empty ~f:(fun acc (name, value) -> set acc name value)
;;

let pp t =
  if Package_variable_name.Map.is_empty t
  then Pp.text "(empty)"
  else
    Pp.enumerate (Package_variable_name.Map.to_list t) ~f:(fun (variable, value) ->
      Pp.textf
        "%s = %s"
        (Package_variable_name.to_string variable)
        (String.maybe_quoted (Variable_value.to_string value)))
;;

let unset = Package_variable_name.Map.remove

let unset_multi t variable_names =
  Package_variable_name.Set.fold variable_names ~init:t ~f:(fun variable_name t ->
    unset t variable_name)
;;

let to_env t variable =
  match OpamVariable.Full.scope variable with
  | Self | Package _ -> None
  | Global ->
    let variable_name =
      OpamVariable.Full.variable variable |> Package_variable_name.of_opam
    in
    get t variable_name |> Option.map ~f:Variable_value.to_opam_variable_contents
;;
