open Import

module T = struct
  type t = Variable_value.t Package_variable_name.Map.t

  let to_dyn = Package_variable_name.Map.to_dyn Variable_value.to_dyn
  let equal = Package_variable_name.Map.equal ~equal:Variable_value.equal
  let compare = Package_variable_name.Map.compare ~compare:Variable_value.compare
end

include T
include Comparable.Make (T)

let repr =
  Repr.view
    (Repr.list (Repr.pair Package_variable_name.repr Variable_value.repr))
    ~to_:Package_variable_name.Map.to_list
;;

let hash t =
  Package_variable_name.Map.foldi t ~init:0 ~f:(fun key value running_hash ->
    Tuple.T3.hash
      Package_variable_name.hash
      Variable_value.hash
      Int.hash
      (key, value, running_hash))
;;

let empty = Package_variable_name.Map.empty
let is_empty = Package_variable_name.Map.is_empty

let is_subset =
  Package_variable_name.Map.is_subset ~f:(fun value ~of_ ->
    Variable_value.equal value of_)
;;

let validate t ~loc =
  match Package_variable_name.Map.find t Package_variable_name.with_test with
  | Some v ->
    if Variable_value.equal v Variable_value.true_
    then
      User_error.raise
        ?loc
        [ Pp.textf
            "Setting the %S solver variable to 'true' is not currently supported. Dune \
             already uses %S for local packages, but it cannot be applied to transitive \
             dependencies."
            Package_variable_name.(to_string with_test)
            Package_variable_name.(to_string with_test)
        ]
    else ()
  | None -> ()
;;

let encode t =
  let open Encoder in
  Package_variable_name.Map.to_list t
  |> list (pair Package_variable_name.encode Variable_value.encode)
;;

let decode_bindings =
  let open Decoder in
  let+ loc, bindings =
    located (repeat (pair Package_variable_name.decode Variable_value.decode))
  in
  match Package_variable_name.Map.of_list bindings with
  | Ok t -> t
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

let decode =
  let open Decoder in
  let+ loc, t = located decode_bindings in
  validate t ~loc:(Some loc);
  t
;;

let decode_workspace =
  let open Decoder in
  let+ loc, t = located decode_bindings in
  Package_variable_name.Map.iteri t ~f:(fun name _ ->
    if Package_variable_name.Set.mem Package_variable_name.platform_specific name
    then
      User_error.raise
        ~loc
        [ Pp.textf
            "Platform variable %S cannot be overridden in workspace_solver_env."
            (Package_variable_name.to_string name)
        ]);
  t
;;

let set = Package_variable_name.Map.set
let get = Package_variable_name.Map.find
let extend a b = Package_variable_name.Map.superpose b a
let contains = Package_variable_name.Map.mem

module Defaults = struct
  let shared =
    [ ( Package_variable_name.opam_version
      , OpamVersion.to_string OpamVersion.current |> Variable_value.string )
    ; Package_variable_name.with_test, Variable_value.false_
    ; Package_variable_name.with_doc, Variable_value.false_
    ; Package_variable_name.with_dev_setup, Variable_value.false_
    ; Package_variable_name.post, Variable_value.true_
    ]
    |> List.fold_left ~init:empty ~f:(fun acc (name, value) -> set acc name value)
  ;;

  let workspace =
    Package_variable_name.Map.singleton
      Package_variable_name.with_test
      Variable_value.true_
  ;;

  let legacy = Package_variable_name.Map.remove shared Package_variable_name.with_test
end

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

let pp_oneline t =
  if Package_variable_name.Map.is_empty t
  then Pp.text "(empty)"
  else
    Pp.concat
      ~sep:(Pp.text "; ")
      (List.map (Package_variable_name.Map.to_list t) ~f:(fun (variable, value) ->
         Pp.textf
           "%s = %s"
           (Package_variable_name.to_string variable)
           (String.maybe_quoted (Variable_value.to_string value))))
;;

let unset = Package_variable_name.Map.remove

let unset_multi t variable_names =
  Package_variable_name.Set.fold variable_names ~init:t ~f:(fun variable_name t ->
    unset t variable_name)
;;

let combine defaults ~current_system ~context ~unset =
  let t =
    [ current_system; context ]
    |> List.filter_opt
    |> List.fold_left ~init:defaults ~f:extend
  in
  match unset with
  | None -> t
  | Some variables -> unset_multi t variables
;;

let remove_all_except t variable_names =
  Package_variable_name.Map.foldi t ~init:t ~f:(fun variable_name _value acc ->
    if Package_variable_name.Set.mem variable_names variable_name
    then acc
    else unset acc variable_name)
;;

let remove_all_except_platform_specific t =
  remove_all_except t Package_variable_name.platform_specific
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

let popular_platform_envs =
  let make ~os ~arch ~os_distribution ~os_family () =
    let env = empty in
    let env = set env Package_variable_name.os (Variable_value.string os) in
    let env =
      match arch with
      | Some arch -> set env Package_variable_name.arch (Variable_value.string arch)
      | None -> env
    in
    let env =
      match os_distribution with
      | Some os_distribution ->
        set
          env
          Package_variable_name.os_distribution
          (Variable_value.string os_distribution)
      | None -> env
    in
    let env =
      match os_family with
      | Some os_family ->
        set env Package_variable_name.os_family (Variable_value.string os_family)
      | None -> env
    in
    env
  in
  [ make ~os:"linux" ~arch:(Some "x86_64") ~os_distribution:None ~os_family:None ()
  ; make ~os:"linux" ~arch:(Some "arm64") ~os_distribution:None ~os_family:None ()
  ; make ~os:"macos" ~arch:(Some "x86_64") ~os_distribution:None ~os_family:None ()
  ; make ~os:"macos" ~arch:(Some "arm64") ~os_distribution:None ~os_family:None ()
  ]
;;

let add_sentinel_values_for_unset_platform_vars solver_env =
  Package_variable_name.Set.fold
    Package_variable_name.platform_specific
    ~init:solver_env
    ~f:(fun name acc ->
      if contains acc name
      then acc
      else set acc name (Variable_value.sentinel_value_of_variable_name name))
;;

module Scope = struct
  type t =
    | Workspace
    | Dependencies

  let of_package ~local_packages name =
    if Dune_lang.Package_name.Map.mem local_packages name then Workspace else Dependencies
  ;;
end

module Scoped = struct
  type env = t

  type t =
    | Shared of env
    | Scoped of
        { workspace : env
        ; dependencies : env
        ; workspace_overrides : env
        }

  let resolve ~current_system ~shared ~workspace ~unset =
    let resolve defaults = combine defaults ~current_system ~context:shared ~unset in
    match workspace with
    | None -> Shared (resolve Defaults.legacy)
    | Some workspace ->
      Scoped
        { workspace =
            extend (resolve (extend Defaults.shared Defaults.workspace)) workspace
        ; dependencies = resolve Defaults.shared
        ; workspace_overrides = workspace
        }
  ;;

  let is_scoped = function
    | Shared _ -> false
    | Scoped _ -> true
  ;;

  let get t scope =
    match t, scope with
    | Shared env, _ -> env
    | Scoped { workspace; _ }, Scope.Workspace -> workspace
    | Scoped { dependencies; _ }, Scope.Dependencies -> dependencies
  ;;

  let for_platform t scope platform =
    let env = extend (get t scope) platform in
    match t, scope with
    | Scoped { workspace_overrides; _ }, Scope.Workspace -> extend env workspace_overrides
    | Shared _, _ | Scoped _, Dependencies -> env
  ;;

  let encode t ~platforms =
    let environments scope =
      List.map platforms ~f:(fun platform -> encode (for_platform t scope platform))
    in
    let open Encoder in
    match t with
    | Shared _ -> list sexp (string "shared" :: environments Dependencies)
    | Scoped _ ->
      list
        sexp
        [ list sexp (string "workspace" :: environments Workspace)
        ; list sexp (string "dependencies" :: environments Dependencies)
        ]
  ;;

  let remove_platform_specific = function
    | Shared env -> Shared (unset_multi env Package_variable_name.platform_specific)
    | Scoped { workspace; dependencies; workspace_overrides } ->
      Scoped
        { workspace = unset_multi workspace Package_variable_name.platform_specific
        ; dependencies = unset_multi dependencies Package_variable_name.platform_specific
        ; workspace_overrides
        }
  ;;

  let add_sentinel_values_for_unset_platform_vars = function
    | Shared env -> Shared (add_sentinel_values_for_unset_platform_vars env)
    | Scoped { workspace; dependencies; workspace_overrides } ->
      Scoped
        { workspace = add_sentinel_values_for_unset_platform_vars workspace
        ; dependencies = add_sentinel_values_for_unset_platform_vars dependencies
        ; workspace_overrides
        }
  ;;
end
