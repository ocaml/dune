open Import

type t = { expanded_variables : Solver_env.Variable.Set.t }

let empty = { expanded_variables = Solver_env.Variable.Set.empty }

module Updater = struct
  type stats = t
  type t = stats ref

  let init () = ref empty
  let snapshot t = !t

  let expand_variable t variable =
    let { expanded_variables } = !t in
    t := { expanded_variables = Solver_env.Variable.Set.add expanded_variables variable }
  ;;
end

module Expanded_variable_bindings = struct
  type t =
    { variable_values : (Solver_env.Variable.t * string) list
    ; unset_variables : Solver_env.Variable.t list
    }

  let empty = { variable_values = []; unset_variables = [] }

  let is_empty { variable_values; unset_variables } =
    List.is_empty variable_values && List.is_empty unset_variables
  ;;

  let of_variable_set variables solver_env =
    Solver_env.Variable.Set.to_list variables
    |> List.fold_left ~init:empty ~f:(fun acc variable ->
      match Solver_env.get solver_env variable with
      | String string ->
        { acc with variable_values = (variable, string) :: acc.variable_values }
      | Unset_sys -> { acc with unset_variables = variable :: acc.unset_variables })
  ;;

  let decode =
    let open Decoder in
    fields
      (let+ variable_values =
         field
           "variable_values"
           ~default:[]
           (repeat (pair Solver_env.Variable.decode string))
       and+ unset_variables =
         field "unset_variables" ~default:[] (repeat Solver_env.Variable.decode)
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
            :: List.map ~f:(pair Solver_env.Variable.encode string) variable_values)
       ])
    @
    if List.is_empty unset_variables
    then []
    else
      [ Dune_sexp.List
          (string "unset_variables"
           :: List.map ~f:Solver_env.Variable.encode unset_variables)
      ]
  ;;

  let equal { variable_values; unset_variables } t =
    List.equal
      (Tuple.T2.equal Solver_env.Variable.equal String.equal)
      variable_values
      t.variable_values
    && List.equal Solver_env.Variable.equal unset_variables t.unset_variables
  ;;

  let to_dyn { variable_values; unset_variables } =
    Dyn.record
      [ ( "variable_values"
        , Dyn.list (Tuple.T2.to_dyn Solver_env.Variable.to_dyn Dyn.string) variable_values
        )
      ; "unset_variables", Dyn.list Solver_env.Variable.to_dyn unset_variables
      ]
  ;;

  let to_solver_env { variable_values; unset_variables = _ } =
    (* TODO currently this only supports system variables but this will be
       generalized when the solver gains support for arbitrary variables *)
    let sys =
      List.fold_left
        variable_values
        ~init:Solver_env.Variable.Sys.Bindings.empty
        ~f:(fun acc (variable, value) ->
          match variable with
          | Solver_env.Variable.Sys sys ->
            Solver_env.Variable.Sys.Bindings.set acc sys value
          | Const _ -> acc)
    in
    Solver_env.create ~sys
  ;;
end
