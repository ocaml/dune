open Import

let add_self_to_filter_env package env variable =
  match OpamVariable.Full.scope variable with
  | Self | Package _ -> env variable
  | Global ->
    let var_name = Package_variable_name.of_opam (OpamVariable.Full.variable variable) in
    if Package_variable_name.(equal var_name name)
    then Some (OpamVariable.S (OpamPackage.Name.to_string (OpamPackage.name package)))
    else if Package_variable_name.(equal var_name version)
    then Some (S (OpamPackage.Version.to_string (OpamPackage.version package)))
    else env variable
;;

let opam_env_update_to_env_update (var, env_op, value_string, _) : _ Action.Env_update.t =
  { Action.Env_update.op = env_op
  ; var
  ; value = String_with_vars.make_text Loc.none value_string
  }
;;
