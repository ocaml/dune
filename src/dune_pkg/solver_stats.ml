open Import

type t = { expanded_variables : Package_variable_name.Set.t }

let empty = { expanded_variables = Package_variable_name.Set.empty }

module Updater = struct
  type stats = t
  type t = stats ref

  let init () = ref empty
  let snapshot t = !t

  let expand_variable t variable =
    let { expanded_variables } = !t in
    t
    := { expanded_variables = Package_variable_name.Set.add expanded_variables variable }
  ;;

  let wrap_env t env variable =
    (match OpamVariable.Full.scope variable with
     | Self | Package _ -> ()
     | Global ->
       let variable_name =
         OpamVariable.Full.variable variable |> Package_variable_name.of_opam
       in
       expand_variable t variable_name);
    env variable
  ;;
end

module Expanded_variable_bindings = struct
  type t =
    { variable_values : (Package_variable_name.t * Variable_value.t) list
    ; unset_variables : Package_variable_name.t list
    }

  let empty = { variable_values = []; unset_variables = [] }

  let is_empty { variable_values; unset_variables } =
    List.is_empty variable_values && List.is_empty unset_variables
  ;;

  let of_variable_set variables solver_env =
    Package_variable_name.Set.to_list variables
    |> List.fold_left ~init:empty ~f:(fun acc variable ->
      match Solver_env.get solver_env variable with
      | Some value ->
        { acc with variable_values = (variable, value) :: acc.variable_values }
      | None -> { acc with unset_variables = variable :: acc.unset_variables })
  ;;

  let decode =
    let open Decoder in
    fields
      (let+ variable_values =
         field
           "variable_values"
           ~default:[]
           (repeat (pair Package_variable_name.decode Variable_value.decode))
       and+ unset_variables =
         field "unset_variables" ~default:[] (repeat Package_variable_name.decode)
       in
       { variable_values; unset_variables })
  ;;

  let encode { variable_values; unset_variables } =
    let open Dune_lang.Encoder in
    (if List.is_empty variable_values
     then []
     else
       [ Dune_sexp.List
           (string "variable_values"
            :: List.map
                 ~f:(pair Package_variable_name.encode Variable_value.encode)
                 variable_values)
       ])
    @
    if List.is_empty unset_variables
    then []
    else
      [ Dune_sexp.List
          (string "unset_variables"
           :: List.map ~f:Package_variable_name.encode unset_variables)
      ]
  ;;

  let equal { variable_values; unset_variables } t =
    List.equal
      (Tuple.T2.equal Package_variable_name.equal Variable_value.equal)
      variable_values
      t.variable_values
    && List.equal Package_variable_name.equal unset_variables t.unset_variables
  ;;

  let to_dyn { variable_values; unset_variables } =
    Dyn.record
      [ ( "variable_values"
        , Dyn.list
            (Tuple.T2.to_dyn Package_variable_name.to_dyn Variable_value.to_dyn)
            variable_values )
      ; "unset_variables", Dyn.list Package_variable_name.to_dyn unset_variables
      ]
  ;;

  let to_solver_env { variable_values; unset_variables = _ } =
    List.fold_left variable_values ~init:Solver_env.empty ~f:(fun acc (variable, value) ->
      Solver_env.set acc variable value)
  ;;

  let validate_against_solver_env t solver_env =
    let hints =
      [ Pp.text
          "This can happen if the \"solver_env\" for the lockdir in the dune-workspace \
           file has changed since generating the lockdir. Regenerate the lockdir by \
           running:"
      ; User_message.command "dune pkg lock"
      ]
    in
    List.iter t.variable_values ~f:(fun (variable_name, variable_value) ->
      Option.iter
        (Solver_env.get solver_env variable_name)
        ~f:(fun variable_value_in_env ->
          if not (Variable_value.equal variable_value_in_env variable_value)
          then
            User_error.raise
              [ Pp.textf
                  "The dependency solution relies on the assignment of the solver \
                   variable %S to %S but the solver environment in the workspace would \
                   assign it the value %S."
                  (Package_variable_name.to_string variable_name)
                  (Variable_value.to_string variable_value)
                  (Variable_value.to_string variable_value_in_env)
              ]
              ~hints));
    List.iter t.unset_variables ~f:(fun variable_name ->
      Option.iter (Solver_env.get solver_env variable_name) ~f:(fun variable_value ->
        User_error.raise
          [ Pp.textf
              "The dependency solution relies on the variable %S not being assigned a \
               value but the solver environment in the workspace would assign it the \
               value %S."
              (Package_variable_name.to_string variable_name)
              (Variable_value.to_string variable_value)
          ]
          ~hints))
  ;;
end
