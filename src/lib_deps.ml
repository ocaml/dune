open Stdune
open Dune_file
open Build_system

let lib_files_alias ~dir ~name ~ext =
  Alias.make (sprintf "lib-%s%s-all" (Lib_name.to_string name) ext) ~dir

let setup_file_deps_alias t ~dir ~ext lib files =
  Super_context.add_alias_deps t
    (lib_files_alias ~dir ~name:(Library.best_name lib) ~ext) files

let setup_file_deps_group_alias t ~dir ~exts lib =
  setup_file_deps_alias t lib ~dir
    ~ext:(String.concat exts ~sep:"-and-")
    (List.map exts ~f:(fun ext ->
       Alias.stamp_file
         (lib_files_alias ~dir ~name:(Library.best_name lib) ~ext))
     |> Path.Set.of_list)

module L = struct
  let file_deps_of_lib t (lib : Lib.t) ~ext =
    if Lib.is_local lib then
      Alias.stamp_file
        (lib_files_alias ~dir:(Lib.src_dir lib) ~name:(Lib.name lib) ~ext)
    else
      Build_system.stamp_file_for_files_of (Super_context.build_system t)
        ~dir:(Lib.obj_dir lib) ~ext

  let file_deps_with_exts t lib_exts =
    List.rev_map lib_exts ~f:(fun (lib, ext) -> file_deps_of_lib t lib ~ext)

  let file_deps t libs ~ext =
    List.rev_map libs ~f:(file_deps_of_lib t ~ext)
end
