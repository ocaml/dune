open Stdune

module Group = struct
  type t =
    | Cmi
    | Cmx
    | Header

  let to_string = function
    | Cmi -> ".cmi"
    | Cmx -> ".cmx"
    | Header -> ".h"
end

let file_deps_of_lib (lib : Lib.t) ~groups =
  let obj_dir = Lib.obj_dir lib in
  List.map groups ~f:(fun (group : Group.t) ->
    let dir =
      match group with
      | Cmi -> Obj_dir.all_cmis_dir obj_dir
      | Cmx -> Obj_dir.native_dir obj_dir
      | Header -> Obj_dir.obj_dir obj_dir
    in
    let ext = Group.to_string group in
    Build_system.stamp_file_for_files_of ~dir ~ext)

let file_deps_with_exts =
  List.concat_map ~f:(fun (lib, groups) -> file_deps_of_lib lib ~groups)

let file_deps libs ~groups =
  List.concat_map libs ~f:(file_deps_of_lib ~groups)
