open Import

type t = Variable_value.t Variable_name.Map.t

let empty = Variable_name.Map.empty
let equal = Variable_name.Map.equal ~equal:Variable_value.equal
let to_dyn = Variable_name.Map.to_dyn Variable_value.to_dyn
let is_empty = Variable_name.Map.is_empty

let decode =
  let open Decoder in
  let+ loc, bindings =
    located (repeat (pair Variable_name.decode Variable_value.decode))
  in
  match Variable_name.Map.of_list bindings with
  | Ok t -> t
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

let set = Variable_name.Map.set
let get = Variable_name.Map.find
let extend = Variable_name.Map.superpose

let with_opam_version_set_to_current =
  set
    empty
    Variable_name.opam_version
    (OpamVersion.to_string OpamVersion.current |> Variable_value.string)
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
