open Import
open Action_builder.O

type t =
  { dir : Path.Build.t
  ; per_module : Module.t list Action_builder.t Module_name.Unique.Map.t
  }

let make ~dir ~per_module = { dir; per_module }

let deps_of t (m : Module.t) =
  match Module_name.Unique.Map.find t.per_module (Module.obj_name m) with
  | Some x -> x
  | None ->
    Code_error.raise
      "Ocamldep.Dep_graph.deps_of"
      [ "dir", Path.Build.to_dyn t.dir
      ; ( "modules"
        , Dyn.(list Module_name.Unique.to_dyn) (Module_name.Unique.Map.keys t.per_module)
        )
      ; "m", Module.to_dyn m
      ]
;;

module Top_closure = Top_closure.Make (Module_name.Unique.Set) (Action_builder)

let top_closed t modules =
  let+ res =
    Top_closure.top_closure modules ~key:Module.obj_name ~deps:(fun m ->
      Module_name.Unique.Map.find_exn t.per_module (Module.obj_name m))
  in
  match res with
  | Ok modules -> modules
  | Error cycle ->
    User_error.raise
      [ Pp.textf "dependency cycle between modules in %s:" (Path.Build.to_string t.dir)
      ; Pp.chain cycle ~f:(fun m -> Pp.verbatim (Module_name.to_string (Module.name m)))
      ]
;;

let top_closed_implementations t modules =
  List.filter modules ~f:(Module.has ~ml_kind:Impl)
  |> top_closed t
  |> Action_builder.map
       ~f:
         (let obj_map =
            List.map modules ~f:(fun x -> x, x)
            |> Module.Obj_map.of_list_reduce ~f:(fun x y ->
              match Module.kind x with
              | Impl_vmodule -> x
              | _ -> y)
          in
          List.filter_map ~f:(fun m ->
            match Module.kind m with
            | Virtual -> Module.Obj_map.find obj_map m
            | Intf_only -> None
            | _ -> Some m))
  |> Action_builder.memoize "top sorted implementations"
;;

let dummy (m : Module.t) =
  { dir = Path.Build.root
  ; per_module =
      Module_name.Unique.Map.singleton (Module.obj_name m) (Action_builder.return [])
  }
;;

module Ml_kind = struct
  type nonrec t = t Ml_kind.Dict.t

  let dummy m = Ml_kind.Dict.make_both (dummy m)

  let for_module_compilation ~modules ({ impl; intf } : t) =
    let dedupe_modules modules =
      let _, modules =
        List.fold_left
          modules
          ~init:(Module_name.Unique.Set.empty, [])
          ~f:(fun (seen, acc) module_ ->
            let obj_name = Module.obj_name module_ in
            if Module_name.Unique.Set.mem seen obj_name
            then seen, acc
            else Module_name.Unique.Set.add seen obj_name, module_ :: acc)
      in
      List.rev modules
    in
    let merge_impl_and_intf_deps obj_name impl_deps =
      let intf_deps = Module_name.Unique.Map.find_exn intf.per_module obj_name in
      let open Action_builder.O in
      let+ impl_deps = impl_deps
      and+ intf_deps = intf_deps in
      dedupe_modules (impl_deps @ intf_deps)
    in
    let per_module =
      Modules.With_vlib.obj_map modules
      |> Module_name.Unique.Map.mapi ~f:(fun obj_name sourced_module ->
        let impl_deps = Module_name.Unique.Map.find_exn impl.per_module obj_name in
        match sourced_module with
        | Modules.Sourced_module.Normal module_ when Module.has module_ ~ml_kind:Intf ->
          merge_impl_and_intf_deps obj_name impl_deps
        | Modules.Sourced_module.Normal _
        | Modules.Sourced_module.Imported_from_vlib _
        | Modules.Sourced_module.Impl_of_virtual_module _ -> impl_deps)
    in
    Ml_kind.Dict.make ~impl:{ impl with per_module } ~intf
  ;;
end
