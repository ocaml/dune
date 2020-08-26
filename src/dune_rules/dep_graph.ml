open! Dune_engine
open! Stdune
open Import
open Build.O

type t =
  { dir : Path.Build.t
  ; per_module : Module.t list Build.t Module.Obj_map.t
  }

let make ~dir ~per_module = { dir; per_module }

let deps_of t (m : Module.t) =
  match Module.Obj_map.find t.per_module m with
  | Some x -> x
  | None ->
    Code_error.raise "Ocamldep.Dep_graph.deps_of"
      [ ("dir", Path.Build.to_dyn t.dir)
      ; ( "modules"
        , Dyn.Encoder.(list Module_name.Unique.to_dyn)
            (Module.Obj_map.keys t.per_module |> List.map ~f:Module.obj_name) )
      ; ("m", Module.to_dyn m)
      ]

let top_closed t modules =
  let+ per_module =
    Module.Obj_map.to_list t.per_module
    |> List.map ~f:(fun (unit, deps) ->
           let+ deps = deps in
           (unit, deps))
    |> Build.all
  in
  let per_module = Module.Obj_map.of_list_exn per_module in
  match Module.Obj_map.top_closure per_module modules with
  | Ok modules -> modules
  | Error cycle ->
    User_error.raise
      [ Pp.textf "dependency cycle between modules in %s:"
          (Path.Build.to_string t.dir)
      ; Pp.chain cycle ~f:(fun m ->
            Pp.verbatim (Module_name.to_string (Module.name m)))
      ]

let top_closed_implementations t modules =
  Build.memoize "top sorted implementations"
    (let filter_out_intf_only = List.filter ~f:(Module.has ~ml_kind:Impl) in
     Build.map
       (top_closed t (filter_out_intf_only modules))
       ~f:filter_out_intf_only)

let dummy (m : Module.t) =
  { dir = Path.Build.root
  ; per_module = Module.Obj_map.singleton m (Build.return [])
  }

module Ml_kind = struct
  type nonrec t = t Ml_kind.Dict.t

  let dummy m = Ml_kind.Dict.make_both (dummy m)
end
