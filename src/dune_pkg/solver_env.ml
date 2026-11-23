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

(* The Dune language version from which "with-dev-setup" is enabled by default,
   and "with-doc" and "with-dev-setup" only apply to local packages. *)
let doc_and_dev_setup_enabled_since = 3, 26

(* Setting [variables] to "true" is the same as leaving them unset: they then
   only apply to local packages. *)
let unset_true t variables =
  Package_variable_name.Map.filteri t ~f:(fun variable value ->
    not
      (Package_variable_name.Set.mem variables variable
       && Variable_value.equal value Variable_value.true_))
;;

let with_test_only = Package_variable_name.Set.singleton Package_variable_name.with_test

let normalize ~dune_version t =
  unset_true
    t
    (if dune_version >= doc_and_dev_setup_enabled_since
     then Package_variable_name.local_only
     else with_test_only)
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
  | Ok t -> unset_true t with_test_only
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

let set t variable_name variable_value =
  unset_true (Package_variable_name.Map.set t variable_name variable_value) with_test_only
;;

let get = Package_variable_name.Map.find
let extend a b = Package_variable_name.Map.superpose b a
let contains = Package_variable_name.Map.mem

let with_defaults ~dune_version =
  ([ ( Package_variable_name.opam_version
     , OpamVersion.to_string OpamVersion.current |> Variable_value.string )
   ; Package_variable_name.post, Variable_value.true_
   ; Package_variable_name.with_doc, Variable_value.false_
   ]
   @
   if dune_version >= doc_and_dev_setup_enabled_since
   then []
   else [ Package_variable_name.with_dev_setup, Variable_value.false_ ])
  |> List.fold_left ~init:empty ~f:(fun acc (name, value) -> set acc name value)
;;

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

let combine ~dune_version ~current_system ~context ~unset =
  let t =
    [ current_system; context ]
    |> List.filter_opt
    |> List.fold_left ~init:(with_defaults ~dune_version) ~f:extend
  in
  let t = normalize ~dune_version t in
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
