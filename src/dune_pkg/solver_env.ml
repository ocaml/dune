open Import

type t = Variable_value.t Variable_name.Map.t

let empty = Variable_name.Map.empty
let equal = Variable_name.Map.equal ~equal:Variable_value.equal
let to_dyn = Variable_name.Map.to_dyn Variable_value.to_dyn
let is_empty = Variable_name.Map.is_empty

let validate t ~loc =
  if Variable_name.Map.mem t Variable_name.with_test
  then
    User_error.raise
      ?loc
      [ Pp.textf
          "Setting the %S solver variable is not permitted as it would conflict with \
           dune's internal use of %S while solving opam packages."
          Variable_name.(to_string with_test)
          Variable_name.(to_string with_test)
      ]
;;

let decode =
  let open Decoder in
  let+ loc, bindings =
    located (repeat (pair Variable_name.decode Variable_value.decode))
  in
  match Variable_name.Map.of_list bindings with
  | Ok t ->
    validate t ~loc:(Some loc);
    t
  | Error (duplicate_key, a, b) ->
    User_error.raise
      ~loc
      [ Pp.textf
          "Duplicate entries for user variable %s (%s, %s)"
          (String.maybe_quoted (Variable_name.to_string duplicate_key))
          (String.maybe_quoted (Variable_value.to_string a))
          (String.maybe_quoted (Variable_value.to_string b))
      ]
;;

let set t variable_name variable_value =
  let t = Variable_name.Map.set t variable_name variable_value in
  validate t ~loc:None;
  t
;;

let get = Variable_name.Map.find
let extend a b = Variable_name.Map.superpose b a

let with_defaults =
  [ ( Variable_name.opam_version
    , OpamVersion.to_string OpamVersion.current |> Variable_value.string )
  ; Variable_name.with_doc, Variable_value.false_
  ]
  |> List.fold_left ~init:empty ~f:(fun acc (name, value) -> set acc name value)
;;

let pp t =
  if Variable_name.Map.is_empty t
  then Pp.text "(empty)"
  else
    Pp.enumerate (Variable_name.Map.to_list t) ~f:(fun (variable, value) ->
      Pp.textf
        "%s = %s"
        (Variable_name.to_string variable)
        (String.maybe_quoted (Variable_value.to_string value)))
;;
