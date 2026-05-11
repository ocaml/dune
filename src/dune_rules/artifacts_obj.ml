open Import
open Memo.O

type t =
  { stanza_dir : Path.Build.t
  ; libraries : Lib_info.local Lib_name.Map.t
  ; modules : (Path.Build.t Obj_dir.t * Module.t) Module_name.Path.Map.t
  }

let empty =
  { stanza_dir = Path.Build.root
  ; libraries = Lib_name.Map.empty
  ; modules = Module_name.Path.Map.empty
  }
;;

let stanza_dir t = t.stanza_dir
let lookup_module { modules; _ } = Module_name.Path.Map.find modules
let lookup_library { libraries; _ } = Lib_name.Map.find libraries

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
        Module_name.Path.Map.add_exn modules (Module.path m) (obj_dir, m))
    in
    let init =
      List.fold_left exes ~init:Module_name.Path.Map.empty ~f:(fun modules (m, obj_dir) ->
        by_path modules obj_dir m)
    in
    List.fold_left libs ~init ~f:(fun modules (_, m, obj_dir) ->
      by_path modules obj_dir m)
  in
  { stanza_dir = dir; libraries; modules }
;;
