open! Stdune
open Import
open Build.O

type t =
  { dir        : Path.t
  ; per_module : (Module.t * (unit, Module.t list) Build.t) Module.Name.Map.t
  }

let make ~dir ~per_module = { dir ; per_module }

let deps_of t (m : Module.t) =
  let name = Module.name m in
  match Module.Name.Map.find t.per_module name with
  | Some (_, x) -> x
  | None ->
    Exn.code_error "Ocamldep.Dep_graph.deps_of"
      [ "dir", Path.to_sexp t.dir
      ; "modules", Sexp.Encoder.(list Module.Name.to_sexp)
                     (Module.Name.Map.keys t.per_module)
      ; "module", Module.Name.to_sexp name
      ]

let pp_cycle fmt cycle =
  (Fmt.list ~pp_sep:Fmt.nl (Fmt.prefix (Fmt.string "-> ") Module.Name.pp))
    fmt (List.map cycle ~f:Module.name)

let top_closed t modules =
  Module.Name.Map.to_list t.per_module
  |> List.map ~f:(fun (unit, (_module, deps)) ->
    deps >>^ fun deps -> (unit, deps))
  |> Build.all
  >>^ fun per_module ->
  let per_module = Module.Name.Map.of_list_exn per_module in
  match
    Module.Name.Top_closure.top_closure modules
      ~key:Module.name
      ~deps:(fun m ->
        Module.name m
        |> Module.Name.Map.find per_module
        |> Option.value_exn)
  with
  | Ok modules -> modules
  | Error cycle ->
    die "dependency cycle between modules in %s:\n   %a"
      (Path.to_string t.dir)
      pp_cycle cycle

module Multi = struct
  let top_closed_multi (ts : t list) modules =
    Format.eprintf "Considering the following modules linking: %a@.%!"
      (Fmt.ocaml_list Module.Name.pp) (List.map ~f:Module.name modules)
    ;
    List.concat_map ts ~f:(fun t ->
      let res =
        Module.Name.Map.to_list t.per_module
        |> List.map ~f:(fun (_name, (unit, deps)) ->
          deps >>^ (fun deps ->
            Format.eprintf "(source: %a) %a: %a@.@."
              Path.pp t.dir
              Module.Name.pp (Module.name unit)
              (Fmt.ocaml_list Module.Name.pp)
              (List.map ~f:Module.name deps);
            (unit, deps))
        )
      in
      Format.eprintf "-------@.";
      res
    )
    |> Build.all >>^ fun per_module ->
    let per_obj =
      Module.Obj_map.of_list_reduce per_module ~f:List.rev_append in
    Format.eprintf "Combined dep graph:@.%!";
    Module.Obj_map.to_list per_obj
    |> List.iter ~f:(fun (m, deps) ->
      Format.eprintf "%a: %a@.@.%!"
        Module.Name.pp (Module.name m)
        (Fmt.ocaml_list Module.Name.pp)
        (List.map ~f:Module.name deps)
    );
    match Module.Obj_map.top_closure per_obj modules with
    | Ok modules ->
      Format.eprintf "Final closure: %a@.%!"
        (Fmt.ocaml_list Module.Name.pp)
        (List.map ~f:Module.name modules)
      ;
      modules
    | Error cycle ->
      die "dependency cycle between modules\n   %a"
        pp_cycle cycle
end

let make_top_closed_implementations ~name ~f ts modules =
  Build.memoize name (
    let filter_out_intf_only = List.filter ~f:Module.has_impl in
    f ts (filter_out_intf_only modules)
    >>^ filter_out_intf_only)

let top_closed_multi_implementations =
  make_top_closed_implementations
    ~name:"top sorted multi implementations" ~f:Multi.top_closed_multi

let top_closed_implementations =
  make_top_closed_implementations
    ~name:"top sorted implementations" ~f:top_closed

let dummy (m : Module.t) =
  { dir = Path.root
  ; per_module =
      Module.Name.Map.singleton (Module.name m) (m, (Build.return []))
  }

let wrapped_compat ~modules ~wrapped_compat =
  { dir = Path.root
  ; per_module = Module.Name.Map.merge wrapped_compat modules ~f:(fun _ d m ->
      match d, m with
      | None, None -> assert false
      | Some wrapped_compat, None ->
        Exn.code_error "deprecated module needs counterpart"
          [ "deprecated", Module.to_sexp wrapped_compat
          ]
      | None, Some _ -> None
      | Some _, Some m -> Some (m, (Build.return [m]))
    )
  }

module Ml_kind = struct
  type nonrec t = t Ml_kind.Dict.t

  let dummy m =
    Ml_kind.Dict.make_both (dummy m)

  let wrapped_compat =
    let w = wrapped_compat in
    fun ~modules ~wrapped_compat ->
      Ml_kind.Dict.make_both (w ~modules ~wrapped_compat)

  let merge_impl ~(ml_kind : Ml_kind.t) _ vlib impl =
    match vlib, impl with
    | None, None -> assert false
    | Some _, None -> None (* we don't care about internal vlib deps *)
    | None, Some d -> Some d
    | Some (mv, _), Some (mi, i) ->
      if Module.obj_name mv = Module.obj_name mi
      && Module.intf_only mv
      && Module.impl_only mi then
        match ml_kind with
        | Impl -> Some (mi, i)
        | Intf -> None
      else if Module.is_private mv || Module.is_private mi then
        Some (mi, i)
      else
        let open Sexp.Encoder in
        Exn.code_error "merge_impl: unexpected dep graph"
          [ "ml_kind", string (Ml_kind.to_string ml_kind)
          ; "mv", Module.to_sexp mv
          ; "mi", Module.to_sexp mi
          ]

  let merge_for_impl ~(vlib : t) ~(impl : t) =
    Ml_kind.Dict.of_func (fun ~ml_kind ->
      let impl = Ml_kind.Dict.get impl ml_kind in
      { impl with
        per_module =
          Module.Name.Map.merge ~f:(merge_impl ~ml_kind)
            (Ml_kind.Dict.get vlib ml_kind).per_module
            impl.per_module
      })
end
