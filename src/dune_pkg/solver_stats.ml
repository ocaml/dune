open Import

type t =
  { workspace : Package_variable_name.Set.t
  ; dependencies : Package_variable_name.Set.t
  }

let empty =
  { workspace = Package_variable_name.Set.empty
  ; dependencies = Package_variable_name.Set.empty
  }
;;

module Updater = struct
  type stats = t
  type t = stats ref

  let init () = ref empty
  let snapshot t = !t

  let expand_variable t ~scope variable =
    let stats = !t in
    t
    := match scope with
       | Solver_env.Scope.Workspace ->
         { stats with workspace = Package_variable_name.Set.add stats.workspace variable }
       | Dependencies ->
         { stats with
           dependencies = Package_variable_name.Set.add stats.dependencies variable
         }
  ;;

  let wrap_env t ~scope env variable =
    (match OpamVariable.Full.scope variable with
     | Self | Package _ -> ()
     | Global ->
       let variable_name =
         OpamVariable.Full.variable variable |> Package_variable_name.of_opam
       in
       expand_variable t ~scope variable_name);
    env variable
  ;;
end

module Bindings = struct
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

  let remove_platform_specific { variable_values; unset_variables } =
    let is_platform_specific variable_name =
      Package_variable_name.Set.mem Package_variable_name.platform_specific variable_name
    in
    { variable_values =
        List.filter variable_values ~f:(fun (variable_name, _) ->
          not (is_platform_specific variable_name))
    ; unset_variables = List.filter unset_variables ~f:(Fun.negate is_platform_specific)
    }
  ;;
end

module Expanded_variable_bindings = struct
  type stats = t

  type t =
    | Shared of Bindings.t
    | Scoped of
        { workspace : Bindings.t
        ; dependencies : Bindings.t
        }

  let empty = Shared Bindings.empty
  let empty_scoped = Scoped { workspace = Bindings.empty; dependencies = Bindings.empty }

  let version = function
    | Shared _ -> 0, 1
    | Scoped _ -> 0, 2
  ;;

  let is_empty = function
    | Shared bindings -> Bindings.is_empty bindings
    | Scoped { workspace; dependencies } ->
      Bindings.is_empty workspace && Bindings.is_empty dependencies
  ;;

  let of_stats ({ workspace; dependencies } : stats) solver_env =
    let bindings scope variables =
      Bindings.of_variable_set variables (Solver_env.Scoped.get solver_env scope)
    in
    if Solver_env.Scoped.is_scoped solver_env
    then
      Scoped
        { workspace = bindings Workspace workspace
        ; dependencies = bindings Dependencies dependencies
        }
    else
      Shared
        (bindings Dependencies (Package_variable_name.Set.union workspace dependencies))
  ;;

  let decode =
    let open Decoder in
    let* version = Syntax.get_exn Dune_lang.Pkg.syntax in
    if version < (0, 2)
    then Bindings.decode >>| fun bindings -> Shared bindings
    else
      fields
        (let+ workspace = field "workspace" Bindings.decode
         and+ dependencies = field "dependencies" Bindings.decode in
         Scoped { workspace; dependencies })
  ;;

  let encode = function
    | Shared bindings -> Bindings.encode bindings
    | Scoped { workspace; dependencies } ->
      let open Encoder in
      [ list sexp (string "workspace" :: Bindings.encode workspace)
      ; list sexp (string "dependencies" :: Bindings.encode dependencies)
      ]
  ;;

  let equal a b =
    match a, b with
    | Shared a, Shared b -> Bindings.equal a b
    | Scoped a, Scoped b ->
      Bindings.equal a.workspace b.workspace
      && Bindings.equal a.dependencies b.dependencies
    | Shared _, Scoped _ | Scoped _, Shared _ -> false
  ;;

  let repr =
    Repr.variant
      "expanded-solver-variable-bindings"
      [ Repr.case "Shared" Bindings.repr ~proj:(function
          | Shared bindings -> Some bindings
          | Scoped _ -> None)
      ; Repr.case "Scoped" (Repr.pair Bindings.repr Bindings.repr) ~proj:(function
          | Scoped { workspace; dependencies } -> Some (workspace, dependencies)
          | Shared _ -> None)
      ]
  ;;

  let to_dyn = Repr.to_dyn repr

  let to_solver_env t ~scope =
    (match t, scope with
     | Shared bindings, _ -> bindings
     | Scoped { workspace; _ }, Solver_env.Scope.Workspace -> workspace
     | Scoped { dependencies; _ }, Dependencies -> dependencies)
    |> Bindings.to_solver_env
  ;;

  let validate_against_solver_env t ~legacy ~solver_env =
    let hints = [ User_message.command "dune pkg lock" ] in
    let validate scope (bindings : Bindings.t) =
      let env = Solver_env.Scoped.get solver_env scope in
      let check name expected =
        let actual = Solver_env.get env name in
        if not (Option.equal Variable_value.equal expected actual)
        then (
          let describe = function
            | None -> "unset"
            | Some value -> String.maybe_quoted (Variable_value.to_string value)
          in
          let scope =
            match scope with
            | Solver_env.Scope.Workspace -> "workspace"
            | Dependencies -> "dependency"
          in
          User_error.raise
            ~hints
            [ Pp.textf
                "The solver environment for %s packages has changed: variable %S was %s \
                 and is now %s. Regenerate the lock directory."
                scope
                (Package_variable_name.to_string name)
                (describe expected)
                (describe actual)
            ])
      in
      List.iter bindings.variable_values ~f:(fun (name, value) -> check name (Some value));
      List.iter bindings.unset_variables ~f:(fun name -> check name None)
    in
    match t, Solver_env.Scoped.is_scoped solver_env with
    | Shared bindings, false -> Bindings.validate_against_solver_env bindings legacy
    | Scoped { workspace; dependencies }, true ->
      validate Workspace workspace;
      validate Dependencies dependencies
    | Shared _, true | Scoped _, false ->
      User_error.raise
        ~hints
        [ Pp.text
            "The workspace_solver_env configuration has changed since this lock \
             directory was generated. Regenerate the lock directory."
        ]
  ;;

  let remove_platform_specific = function
    | Shared bindings -> Shared (Bindings.remove_platform_specific bindings)
    | Scoped { workspace; dependencies } ->
      Scoped
        { workspace = Bindings.remove_platform_specific workspace
        ; dependencies = Bindings.remove_platform_specific dependencies
        }
  ;;
end
