open Stdune

type t =
  { obj_dir : Path.Build.t Obj_dir.t
  ; modules : Module.t list
  ; top_sorted_modules : Module.t list Build.t
  ; ext_obj : string
  }

let make ~obj_dir ~modules ~top_sorted_modules ~ext_obj =
  let modules = Modules.impl_only modules in
  { obj_dir; modules; top_sorted_modules; ext_obj }

let objects_and_cms t ~mode modules =
  let kind = Mode.cm_kind mode in
  let cm_files = Obj_dir.Module.L.cm_files t.obj_dir modules ~kind in
  match mode with
  | Byte -> cm_files
  | Native ->
    Obj_dir.Module.L.o_files t.obj_dir modules ~ext_obj:t.ext_obj
    |> List.rev_append cm_files

let unsorted_objects_and_cms t ~mode = objects_and_cms t ~mode t.modules

let top_sorted_cms t ~mode =
  let kind = Mode.cm_kind mode in
  Build.map t.top_sorted_modules ~f:(Obj_dir.Module.L.cm_files t.obj_dir ~kind)

let top_sorted_objects_and_cms t ~mode =
  Build.map t.top_sorted_modules ~f:(objects_and_cms t ~mode)
