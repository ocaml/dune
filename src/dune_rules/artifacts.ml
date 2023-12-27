open Import
open Memo.O

let bin_dir_basename = ".bin"
let local_bin p = Path.Build.relative p bin_dir_basename

type t =
  { context : Context.t
  ; (* Mapping from executable names to their actual path in the workspace.
       The keys are the executable names without the .exe, even on Windows.
       Enumerating binaries from install stanzas may involve expanding globs,
       but the artifacts database is depended on by the logic which expands
       globs. The computation of this field is deferred to break the cycle. *)
    local_bins : Path.Build.t Filename.Map.t Memo.Lazy.t
  }

let force { local_bins; _ } =
  let+ (_ : Path.Build.t Filename.Map.t) = Memo.Lazy.force local_bins in
  ()
;;

let analyze_binary t name =
  match Filename.is_relative name with
  | false -> Memo.return (Some (Path.of_filename_relative_to_initial_cwd name))
  | true ->
    let* local_bins = Memo.Lazy.force t.local_bins in
    (match Filename.Map.find local_bins name with
     | Some path -> Memo.return (Some (Path.build path))
     | None -> Context.which t.context name)
;;

let binary t ?hint ~loc name =
  analyze_binary t name
  >>| function
  | Some path -> Ok path
  | None ->
    let context = Context.name t.context in
    Error (Action.Prog.Not_found.create ~program:name ?hint ~context ~loc ())
;;

let binary_available t name =
  analyze_binary t name
  >>= function
  | None -> Memo.return false
  | Some path ->
    (match path with
     | External e -> Fs_memo.file_exists @@ External e
     | In_source_tree e -> Fs_memo.file_exists @@ In_source_dir e
     | In_build_dir _ -> Memo.return true)
;;

let add_binaries t ~dir l =
  let local_bins =
    Memo.lazy_ ~name:"Artifacts.Bin.add_binaries" (fun () ->
      let+ local_bins = Memo.Lazy.force t.local_bins in
      List.fold_left l ~init:local_bins ~f:(fun acc fb ->
        let path = File_binding.Expanded.dst_path fb ~dir:(local_bin dir) in
        Filename.Map.set acc (Path.Build.basename path) path))
  in
  { t with local_bins }
;;

let create =
  let drop_suffix name =
    if Sys.win32
    then Option.value ~default:name (String.drop_suffix name ~suffix:".exe")
    else name
  in
  fun (context : Context.t) ~local_bins ->
    let local_bins =
      Memo.lazy_ (fun () ->
        let+ local_bins = Memo.Lazy.force local_bins in
        Path.Build.Set.fold local_bins ~init:Filename.Map.empty ~f:(fun path acc ->
          let name = Path.Build.basename path in
          let key = drop_suffix name in
          Filename.Map.set acc key path))
    in
    { context; local_bins }
;;

module Objs = struct
  type t =
    { libraries : Lib_info.local Lib_name.Map.t
    ; modules : (Path.Build.t Obj_dir.t * Module.t) Module_name.Map.t
    }

  let empty = { libraries = Lib_name.Map.empty; modules = Module_name.Map.empty }
  let lookup_module { modules; libraries = _ } = Module_name.Map.find modules
  let lookup_library { libraries; modules = _ } = Lib_name.Map.find libraries

  let make ~dir ~lib_config ~libs ~exes =
    let+ libraries =
      Memo.List.map libs ~f:(fun ((lib : Dune_file.Library.t), _, _, _) ->
        let* lib_config = lib_config in
        let name = Lib_name.of_local lib.name in
        let+ info = Dune_file.Library.to_lib_info lib ~dir ~lib_config in
        name, info)
      >>| Lib_name.Map.of_list_exn
    in
    let modules =
      let by_name modules obj_dir =
        Modules.fold_user_available ~init:modules ~f:(fun m modules ->
          Module_name.Map.add_exn modules (Module.name m) (obj_dir, m))
      in
      let init =
        List.fold_left
          exes
          ~init:Module_name.Map.empty
          ~f:(fun modules (_, _, m, obj_dir) -> by_name modules obj_dir m)
      in
      List.fold_left libs ~init ~f:(fun modules (_, _, m, obj_dir) ->
        by_name modules obj_dir m)
    in
    { libraries; modules }
  ;;
end
