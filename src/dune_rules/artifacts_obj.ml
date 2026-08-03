open Import
open Memo.O

type t =
  { libraries : Lib_info.local Lib_name.Map.t
  ; modules : (Path.Build.t Obj_dir.t * Module.t) Path.Build.Map.t
  }

let empty = { libraries = Lib_name.Map.empty; modules = Path.Build.Map.empty }
let lookup_module { modules; _ } = Path.Build.Map.find modules
let lookup_library { libraries; _ } = Lib_name.Map.find libraries

(* The source file build path of a module, with the [.ml]/[.mli] extension
   stripped. This matches the form a user writes in a module artifact pform
   (e.g. [%{cmi:sub_a/group}] for a source file [sub_a/group.ml]). *)
let module_source_path_without_extension m =
  let source =
    match Module.source_without_pp m ~ml_kind:Impl with
    | Some _ as p -> p
    | None -> Module.source_without_pp m ~ml_kind:Intf
  in
  Option.bind source ~f:Path.as_in_build_dir
  |> Option.map ~f:(fun p -> fst (Path.Build.split_extension p))
;;

let make ~dir ~expander ~lib_config ~libs ~exes =
  let+ libraries =
    Memo.List.map libs ~f:(fun ((lib : Library.t), _, _) ->
      let+ lib_config = lib_config in
      let name = Lib_name.of_local lib.name in
      let info =
        Library.to_lib_info lib ~expander:(Memo.return expander) ~dir ~lib_config
      in
      name, info)
    >>| Lib_name.Map.of_list_exn
  in
  let modules =
    let by_path modules obj_dir =
      Modules.fold_user_available ~init:modules ~f:(fun m modules ->
        match module_source_path_without_extension m with
        | None -> modules
        | Some key -> Path.Build.Map.add_exn modules key (obj_dir, m))
    in
    let init =
      List.fold_left exes ~init:Path.Build.Map.empty ~f:(fun modules (m, obj_dir) ->
        by_path modules obj_dir m)
    in
    List.fold_left libs ~init ~f:(fun modules (_, m, obj_dir) ->
      by_path modules obj_dir m)
  in
  { libraries; modules }
;;
