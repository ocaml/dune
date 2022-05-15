open Import
open Action_builder.O

type t =
  { dir : Path.Build.t
  ; per_module : Module.t list Action_builder.t Module.Obj_map.t
  }

let make ~dir ~per_module = { dir; per_module }

let deps_of t (m : Module.t) =
  match Module.Obj_map.find t.per_module m with
  | Some x -> x
  | None ->
    Code_error.raise "Ocamldep.Dep_graph.deps_of"
      [ ("dir", Path.Build.to_dyn t.dir)
      ; ( "modules"
        , Dyn.(list Module_name.Unique.to_dyn)
            (Module.Obj_map.keys t.per_module |> List.map ~f:Module.obj_name) )
      ; ("m", Module.to_dyn m)
      ]

module Top_closure = Top_closure.Make (Module_name.Unique.Set) (Action_builder)

let top_closed t modules =
  let+ res =
    Top_closure.top_closure modules ~key:Module.obj_name
      ~deps:(Module.Obj_map.find_exn t.per_module)
  in
  match res with
  | Ok modules -> modules
  | Error cycle ->
    User_error.raise
      [ Pp.textf "dependency cycle between modules in %s:"
          (Path.Build.to_string t.dir)
      ; Pp.chain cycle ~f:(fun m ->
            Pp.verbatim (Module_name.to_string (Module.name m)))
      ]

let top_closed_implementations t modules =
  Action_builder.memoize "top sorted implementations"
    (let filter_out_intf_only = List.filter ~f:(Module.has ~ml_kind:Impl) in
     Action_builder.map
       (top_closed t (filter_out_intf_only modules))
       ~f:filter_out_intf_only)

let dummy (m : Module.t) =
  { dir = Path.Build.root
  ; per_module = Module.Obj_map.singleton m (Action_builder.return [])
  }

module Ml_kind = struct
  type nonrec t = t Ml_kind.Dict.t

  let dummy m = Ml_kind.Dict.make_both (dummy m)
end
