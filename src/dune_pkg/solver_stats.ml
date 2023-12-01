open Import

type t = { expanded_variables : Variable_name.Set.t }

let empty = { expanded_variables = Variable_name.Set.empty }

module Updater = struct
  type stats = t
  type t = stats ref

  let init () = ref empty
  let snapshot t = !t

  let expand_variable t variable =
    let { expanded_variables } = !t in
    t := { expanded_variables = Variable_name.Set.add expanded_variables variable }
  ;;
end

module Expanded_variable_bindings = struct
  type t =
    { variable_values : (Variable_name.t * Variable_value.t) list
    ; unset_variables : Variable_name.t list
    }

  let empty = { variable_values = []; unset_variables = [] }

  let is_empty { variable_values; unset_variables } =
    List.is_empty variable_values && List.is_empty unset_variables
  ;;

  let of_variable_set variables solver_env =
    Variable_name.Set.to_list variables
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
           (repeat (pair Variable_name.decode Variable_value.decode))
       and+ unset_variables =
         field "unset_variables" ~default:[] (repeat Variable_name.decode)
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
                 ~f:(pair Variable_name.encode Variable_value.encode)
                 variable_values)
       ])
    @
    if List.is_empty unset_variables
    then []
    else
      [ Dune_sexp.List
          (string "unset_variables" :: List.map ~f:Variable_name.encode unset_variables)
      ]
  ;;

  let equal { variable_values; unset_variables } t =
    List.equal
      (Tuple.T2.equal Variable_name.equal Variable_value.equal)
      variable_values
      t.variable_values
    && List.equal Variable_name.equal unset_variables t.unset_variables
  ;;

  let to_dyn { variable_values; unset_variables } =
    Dyn.record
      [ ( "variable_values"
        , Dyn.list
            (Tuple.T2.to_dyn Variable_name.to_dyn Variable_value.to_dyn)
            variable_values )
      ; "unset_variables", Dyn.list Variable_name.to_dyn unset_variables
      ]
  ;;

  let to_solver_env { variable_values; unset_variables = _ } =
    List.fold_left variable_values ~init:Solver_env.empty ~f:(fun acc (variable, value) ->
      Solver_env.set acc variable value)
  ;;
end
