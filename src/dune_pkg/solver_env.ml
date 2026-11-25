open Import

module With = struct
  module Value = struct
    module T = struct
      type t =
        | None
        | Local

      let all = [ "none", None; "local", Local ]
      let enum = Repr.Enum.make "solver-env-with-value" all
      let repr = Repr.Enum.repr enum
    end

    include T
    include Repr.Poly (T)

    let to_dyn = Repr.to_dyn repr
    let to_string = Repr.Enum.to_string enum

    let to_bool t ~local =
      match t with
      | None -> false
      | Local -> local
    ;;
  end

  type 'a t =
    { with_test : 'a
    ; with_doc : 'a
    ; with_dev_setup : 'a
    }

  let repr a =
    Repr.record
      "solver-env-with"
      [ Repr.field "with-test" a ~get:(fun t -> t.with_test)
      ; Repr.field "with-doc" a ~get:(fun t -> t.with_doc)
      ; Repr.field "with-dev-setup" a ~get:(fun t -> t.with_dev_setup)
      ]
  ;;

  let equal equal { with_test; with_doc; with_dev_setup } t =
    equal with_test t.with_test
    && equal with_doc t.with_doc
    && equal with_dev_setup t.with_dev_setup
  ;;

  let compare compare { with_test; with_doc; with_dev_setup } t =
    let open Ordering.O in
    let= () = compare with_test t.with_test in
    let= () = compare with_doc t.with_doc in
    compare with_dev_setup t.with_dev_setup
  ;;

  let hash hash { with_test; with_doc; with_dev_setup } =
    Tuple.T3.hash hash hash hash (with_test, with_doc, with_dev_setup)
  ;;

  let unset = { with_test = None; with_doc = None; with_dev_setup = None }

  let map { with_test; with_doc; with_dev_setup } ~f =
    { with_test = f with_test; with_doc = f with_doc; with_dev_setup = f with_dev_setup }
  ;;

  let encode { with_test; with_doc; with_dev_setup } =
    List.filter_map
      [ Package_variable_name.with_test, with_test
      ; Package_variable_name.with_doc, with_doc
      ; Package_variable_name.with_dev_setup, with_dev_setup
      ]
      ~f:(fun (variable, value) ->
        match value with
        | Value.Local -> Some (Package_variable_name.encode variable)
        | Value.None -> None)
  ;;

  let decode =
    let open Decoder in
    let+ qualifiers = repeat (located Package_variable_name.decode) in
    List.fold_left
      qualifiers
      ~init:{ with_test = Value.None; with_doc = Value.None; with_dev_setup = Value.None }
      ~f:(fun t (loc, variable) ->
        if Package_variable_name.equal variable Package_variable_name.with_test
        then { t with with_test = Value.Local }
        else if Package_variable_name.equal variable Package_variable_name.with_doc
        then { t with with_doc = Value.Local }
        else if Package_variable_name.equal variable Package_variable_name.with_dev_setup
        then { t with with_dev_setup = Value.Local }
        else
          User_error.raise
            ~loc
            [ Pp.textf "Unknown qualifier %S." (Package_variable_name.to_string variable)
            ])
  ;;

  (* The Dune language version from which "with-dev-setup" applies to local
     packages by default. *)
  let dev_setup_enabled_since = 3, 26

  let default ~dune_version =
    { with_test = Value.Local
    ; with_doc = Value.None
    ; with_dev_setup =
        (if dune_version >= dev_setup_enabled_since then Value.Local else Value.None)
    }
  ;;
end

module Variables = struct
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

  let encode t =
    let open Encoder in
    Package_variable_name.Map.to_list t
    |> list (pair Package_variable_name.encode Variable_value.encode)
  ;;

  let decode =
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

  let set = Package_variable_name.Map.set
  let get = Package_variable_name.Map.find
  let mem = Package_variable_name.Map.mem
  let extend a b = Package_variable_name.Map.superpose b a

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

  let remove_all_except t variable_names =
    Package_variable_name.Map.filteri t ~f:(fun variable_name _value ->
      Package_variable_name.Set.mem variable_names variable_name)
  ;;

  let remove_all_except_platform_specific t =
    remove_all_except t Package_variable_name.platform_specific
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

  let to_env t variable =
    match OpamVariable.Full.scope variable with
    | Self | Package _ -> None
    | Global ->
      OpamVariable.Full.variable variable
      |> Package_variable_name.of_opam
      |> get t
      |> Option.map ~f:Variable_value.to_opam_variable_contents
  ;;

  let add_sentinel_values_for_unset_platform_vars t =
    Package_variable_name.Set.fold
      Package_variable_name.platform_specific
      ~init:t
      ~f:(fun name acc ->
        if mem acc name
        then acc
        else set acc name (Variable_value.sentinel_value_of_variable_name name))
  ;;
end

module T = struct
  type t =
    { variables : Variables.t
    ; with_ : With.Value.t With.t
    }

  let to_dyn { variables; with_ } =
    Dyn.record
      [ "variables", Variables.to_dyn variables
      ; "with", Repr.to_dyn (With.repr With.Value.repr) with_
      ]
  ;;

  let equal { variables; with_ } t =
    Variables.equal variables t.variables && With.equal With.Value.equal with_ t.with_
  ;;

  let compare { variables; with_ } t =
    let open Ordering.O in
    let= () = Variables.compare variables t.variables in
    With.compare With.Value.compare with_ t.with_
  ;;
end

include T
include Comparable.Make (T)

let hash { variables; with_ } =
  Tuple.T2.hash Variables.hash (With.hash With.Value.hash) (variables, with_)
;;

let make variables with_ = { variables; with_ }

let pp { variables; with_ } =
  let with_ =
    List.filter
      [ Package_variable_name.with_test, with_.with_test
      ; Package_variable_name.with_doc, with_.with_doc
      ; Package_variable_name.with_dev_setup, with_.with_dev_setup
      ]
      ~f:(fun (variable, _) -> not (Variables.mem variables variable))
  in
  Pp.vbox
    (Pp.concat
       ~sep:Pp.cut
       [ Variables.pp variables
       ; Pp.enumerate with_ ~f:(fun (variable, value) ->
           Pp.textf
             "%s = %s"
             (Package_variable_name.to_string variable)
             (With.Value.to_string value))
       ])
;;

let variables t = t.variables
let with_ t = t.with_
let get t = Variables.get t.variables
let extend t variables = { t with variables = Variables.extend t.variables variables }

let unset_multi t variable_names =
  { t with variables = Variables.unset_multi t.variables variable_names }
;;

let add_sentinel_values_for_unset_platform_vars t =
  { t with variables = Variables.add_sentinel_values_for_unset_platform_vars t.variables }
;;

let without_test t =
  { variables = Variables.unset t.variables Package_variable_name.with_test
  ; with_ = { t.with_ with with_test = With.Value.None }
  }
;;

let default_variables =
  [ ( Package_variable_name.opam_version
    , OpamVersion.to_string OpamVersion.current |> Variable_value.string )
  ; Package_variable_name.post, Variable_value.true_
  ]
  |> List.fold_left ~init:Variables.empty ~f:(fun acc (name, value) ->
    Variables.set acc name value)
;;

let combine ~dune_version ~current_system ~context ~with_ ~unset =
  let variables =
    [ current_system; context ]
    |> List.filter_opt
    |> List.fold_left ~init:default_variables ~f:Variables.extend
  in
  let variables =
    match unset with
    | None -> variables
    | Some unset -> Variables.unset_multi variables unset
  in
  let default = With.default ~dune_version in
  let { With.with_test; with_doc; with_dev_setup } = with_ in
  { variables
  ; with_ =
      { with_test = Option.value with_test ~default:default.with_test
      ; with_doc = Option.value with_doc ~default:default.with_doc
      ; with_dev_setup = Option.value with_dev_setup ~default:default.with_dev_setup
      }
  }
;;

let to_env ~local { variables; with_ } variable =
  match Variables.to_env variables variable with
  | Some _ as value -> value
  | None ->
    (match OpamVariable.Full.scope variable with
     | Self | Package _ -> None
     | Global ->
       let variable_name =
         OpamVariable.Full.variable variable |> Package_variable_name.of_opam
       in
       let value =
         if Package_variable_name.equal variable_name Package_variable_name.with_test
         then Some with_.with_test
         else if Package_variable_name.equal variable_name Package_variable_name.with_doc
         then Some with_.with_doc
         else if
           Package_variable_name.equal variable_name Package_variable_name.with_dev_setup
         then Some with_.with_dev_setup
         else None
       in
       Option.map value ~f:(fun value -> OpamVariable.B (With.Value.to_bool value ~local)))
;;
