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
    ; with_ : Solver_env.With.Value.t Solver_env.With.t
    }

  (* What lockdirs which do not record [with_] were solved with. *)
  let default_with =
    { Solver_env.With.with_test = Solver_env.With.Value.Local
    ; with_doc = None
    ; with_dev_setup = None
    }
  ;;

  let empty = { variable_values = []; unset_variables = []; with_ = default_with }

  let of_variable_set variables solver_env =
    Package_variable_name.Set.to_list variables
    |> List.fold_left
         ~init:{ empty with with_ = Solver_env.with_ solver_env }
         ~f:(fun acc variable ->
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
       and+ with_ = field "with" ~default:default_with (fields Solver_env.With.decode) in
       { variable_values; unset_variables; with_ })
  ;;

  let encode { variable_values; unset_variables; with_ } =
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
    @ (if List.is_empty unset_variables
       then []
       else
         [ Dune_sexp.List
             (string "unset_variables"
              :: List.map ~f:Package_variable_name.encode unset_variables)
         ])
    @ [ Dune_sexp.List (string "with" :: Solver_env.With.encode with_) ]
  ;;

  let equal { variable_values; unset_variables; with_ } t =
    List.equal
      (Tuple.T2.equal Package_variable_name.equal Variable_value.equal)
      variable_values
      t.variable_values
    && List.equal Package_variable_name.equal unset_variables t.unset_variables
    && Solver_env.With.equal Solver_env.With.Value.equal with_ t.with_
  ;;

  let repr =
    Repr.record
      "expanded-variable-bindings"
      [ Repr.field
          "variable_values"
          Repr.(list (pair Package_variable_name.repr Variable_value.repr))
          ~get:(fun t -> t.variable_values)
      ; Repr.field
          "unset_variables"
          Repr.(list Package_variable_name.repr)
          ~get:(fun t -> t.unset_variables)
      ; Repr.field "with" (Solver_env.With.repr Solver_env.With.Value.repr) ~get:(fun t ->
          t.with_)
      ]
  ;;

  let to_dyn = Repr.to_dyn repr

  let to_solver_env { variable_values; unset_variables = _; with_ } =
    Solver_env.make
      (List.fold_left
         variable_values
         ~init:Solver_env.Variables.empty
         ~f:(fun acc (variable, value) -> Solver_env.Variables.set acc variable value))
      with_
  ;;

  let validate_against_solver_env t solver_env =
    let hints =
      [ Pp.text
          "This can happen if the \"solver_env\" or the \"with-*\" fields for the \
           lockdir in the dune-workspace file have changed. Regenerate the lockdir by \
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
          ~hints));
    let with_ = Solver_env.with_ solver_env in
    let validate variable_name value workspace_value =
      if not (Solver_env.With.Value.equal value workspace_value)
      then
        User_error.raise
          [ Pp.textf
              "The dependency solution relies on %S being %s but the workspace sets it \
               to %s."
              (Package_variable_name.to_string variable_name)
              (Solver_env.With.Value.to_string value)
              (Solver_env.With.Value.to_string workspace_value)
          ]
          ~hints
    in
    validate Package_variable_name.with_test t.with_.with_test with_.with_test;
    validate Package_variable_name.with_doc t.with_.with_doc with_.with_doc;
    validate
      Package_variable_name.with_dev_setup
      t.with_.with_dev_setup
      with_.with_dev_setup
  ;;

  let remove_platform_specific { variable_values; unset_variables; with_ } =
    let is_platform_specific variable_name =
      Package_variable_name.Set.mem Package_variable_name.platform_specific variable_name
    in
    { variable_values =
        List.filter variable_values ~f:(fun (variable_name, _) ->
          not (is_platform_specific variable_name))
    ; unset_variables = List.filter unset_variables ~f:(Fun.negate is_platform_specific)
    ; with_
    }
  ;;
end
