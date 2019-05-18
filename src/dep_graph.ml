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

module Vlib_impl = struct
  (* A bit of background information why we need to merge dependency graphs
     of virtual libraries and implementations.

     Before we added virtual libraries, every library had an archive filed
     corresponding to libname.cma and libname.cmxa. This archive file was
     constructed with ocamlopt -a -o libname.cma module1.cmo module2.cmo ...
     where the modules had to be in top sorted order according to the dependency
     graph of the module implementations (what we approximate using ocamldep
     foo.ml).

     Virtual libraries have complicated this situation because they longer have
     an archive file. However, implementations do have an archive file that must
     be constructed from the .cm{o,x}'s of the vlib *and the impl. This means
     that the information required to construct a valid dep graph of all object
     files comes from both the vlib's and impl's dep graphs. This is where the
     need for merging the vlib's and impl's dependency graph for .ml files
     arises.

     The merging procedure must take care to select the correct [Module.t].
     Every virtual module can be present in both the impl's and vlib's
     dependency graphs. The vlib's version has kind = Virtual and the impl's has
     kind = Impl. Clearly, we should be using impl's Module.t as we are trying
     to create an archive from all the object code. *)

  let to_obj_map (t : t) =
    Module.Name.Map.to_list t.per_module
    |> Module.Obj_map.of_list_map_exn ~f:(fun (_name, (m, deps)) ->
      (m, deps))

  let top_closed ~vlib ~impl modules =
    let obj_map_of_list =
      Module.Obj_map.of_list_map_exn ~f:(fun m -> (m, m)) in
    let replace_virtual_module_by_impl =
      let modules_by_obj_map = obj_map_of_list modules in
      fun m ->
        (* this is necessary whenever [m] is virtual. in that case, we need the
           implementation's version b/c it has object file (has_impl = true) *)
        if Module.is_virtual m then
          (* will always exist b/c the implementation satisfies this for every
             virtual module *)
          Module.Obj_map.find_exn modules_by_obj_map m
        else
          m
    in
    Module.Obj_map.merge (to_obj_map vlib) (to_obj_map impl)
      ~f:(fun _ vlib impl ->
        match vlib, impl with
        | None, None -> assert false
        | Some vlib, None ->
          vlib >>^ List.map ~f:replace_virtual_module_by_impl
          |> Option.some
        | None, Some impl -> Some impl
        | Some vlib, Some impl ->
          vlib &&& impl
          >>^ (fun (vlib, impl) ->
            Module.Obj_map.merge
              (obj_map_of_list vlib) (obj_map_of_list impl)
              ~f:(fun _ vlib impl ->
                match vlib, impl with
                | None, None -> assert false
                | Some vlib, None ->
                  Some (replace_virtual_module_by_impl vlib)
                | None, Some impl -> Some impl
                | Some vlib, Some impl ->
                  assert (Module.is_virtual vlib);
                  Some impl)
            |> Module.Obj_map.values)
          |> Option.some)
    |> Module.Obj_map.to_list
    |> List.map ~f:(fun (m, deps) ->
      deps >>^ fun deps -> (m, deps))
    |> Build.all
    >>^ fun per_obj ->
    let per_obj = Module.Obj_map.of_list_exn per_obj in
    match Module.Obj_map.top_closure per_obj modules with
    | Ok modules -> modules
    | Error cycle ->
      die "dependency cycle between modules\n   %a"
        pp_cycle cycle
end

let make_top_closed_implementations ~name ~f modules =
  Build.memoize name (
    let filter_out_intf_only = List.filter ~f:Module.has_impl in
    f (filter_out_intf_only modules)
    >>^ filter_out_intf_only)

let top_closed_implementations_for_vlib_impl ~vlib ~impl =
  make_top_closed_implementations
    ~name:"top sorted implementations for vlib implementation"
    ~f:(Vlib_impl.top_closed ~vlib ~impl)

let top_closed_implementations t =
  make_top_closed_implementations
    ~name:"top sorted implementations" ~f:(top_closed t)

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
