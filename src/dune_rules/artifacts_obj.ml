open Import
open Memo.O

type t =
  { libraries : Lib_info.local Lib_name.Map.t
  ; modules : (Path.Build.t Obj_dir.t * Module.t) Module_name.Map.t
  }

let empty = { libraries = Lib_name.Map.empty; modules = Module_name.Map.empty }
let lookup_module { modules; libraries = _ } = Module_name.Map.find modules
let lookup_library { libraries; modules = _ } = Lib_name.Map.find libraries

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
    let by_name modules obj_dir =
      Modules.fold_user_available ~init:modules ~f:(fun m modules ->
        Module_name.Map.add_exn modules (Module.name m) (obj_dir, m))
    in
    let init =
      List.fold_left exes ~init:Module_name.Map.empty ~f:(fun modules (m, obj_dir) ->
        by_name modules obj_dir m)
    in
    List.fold_left libs ~init ~f:(fun modules (_, m, obj_dir) ->
      by_name modules obj_dir m)
  in
  { libraries; modules }
;;
