open! Stdune

type t =
  { vlib            : Lib.t
  ; impl            : Dune_file.Library.t
  ; obj_dir         : Obj_dir.t
  ; vlib_modules    : Lib_modules.t
  ; vlib_dep_graph  : Dep_graph.Ml_kind.t
  }

let vlib_modules t = t.vlib_modules
let vlib t = t.vlib
let impl t = t.impl
let vlib_dep_graph t = t.vlib_dep_graph

let from_vlib_to_impl_module t m =
  Module.set_obj_dir ~obj_dir:t.obj_dir m

let make ~vlib ~impl ~dir ~vlib_modules ~vlib_dep_graph =
  { impl
  ; obj_dir = Dune_file.Library.obj_dir ~dir impl
  ; vlib
  ; vlib_modules
  ; vlib_dep_graph
  }

let add_vlib_modules t modules =
  match t with
  | None -> modules
  | Some t ->
    Module.Name.Map.superpose (Lib_modules.modules t.vlib_modules) modules

let is_public_vlib_module t m =
  match t with
  | None -> false
  | Some { vlib_modules ; _ } ->
    let modules = Lib_modules.modules vlib_modules in
    begin match Module.Name.Map.find modules (Module.name m) with
    | None -> false
    | Some m -> Module.is_public m
    end

let impl_only = function
  | None -> []
  | Some t ->
    t.vlib_modules |> Lib_modules.modules |> Module.Name_map.impl_only

let aliased_modules t modules =
  match t with
  | None -> Lib_modules.for_alias modules
  | Some t ->
    Module.Name.Map.merge
      (Lib_modules.for_alias modules)
      (Lib_modules.for_alias t.vlib_modules) ~f:(fun _ impl vlib ->
        match impl, vlib with
        | None, None -> assert false
        | Some _, _ -> impl
        | _, Some vlib ->
          let vlib = from_vlib_to_impl_module t vlib in
          Option.some_if (Module.is_public vlib) vlib)

let find_module t m =
  match t with
  | None -> None
  | Some t ->
    Module.name m
    |> Module.Name.Map.find (Lib_modules.modules t.vlib_modules)
    |> Option.map ~f:(from_vlib_to_impl_module t)

let vlib_stubs_o_files = function
  | None -> []
  | Some t -> Lib.foreign_objects t.vlib

let for_file_deps t modules =
  match t with
  | None -> modules
  | Some t ->
    Lib_modules.for_compilation t.vlib_modules
    |> Module.Name.Map.values
    |> List.map ~f:(from_vlib_to_impl_module t)
    |> List.rev_append modules
