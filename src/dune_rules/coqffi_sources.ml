open Import
open Memo.O

let lib ~dir ~library =
  Resolve.Memo.read_memo
  @@ let* scope = Scope.DB.find_by_dir dir in
     let lib_db = Scope.libs scope in
     Lib.DB.resolve lib_db library

let target_of ~dir m = Path.Build.relative dir (Module_name.to_string m ^ ".v")

let modules_of_lib ~loc ~lib ~ml_sources =
  let lib_info = Lib.info lib in
  let lib_name = Lib.name lib in
  match Lib_info.modules lib_info with
  | External (Some m) -> Memo.return m
  | External None ->
    User_error.raise ~loc
      [ Pp.textf
          "Library %S was not installed using Dune and is therefore not \
           supported by the coqffi stanza."
          (Lib_name.to_string lib_name)
      ]
  | Local ->
    ml_sources
    >>| Ml_sources.modules_and_obj_dir ~for_:(Library lib_name)
    >>| fst

let modules_of ~loc ~lib ~modules ~ml_sources =
  let* modules_of_lib = modules_of_lib ~loc ~lib ~ml_sources in
  let parse m =
    match Modules.find modules_of_lib m with
    | Some m -> m
    | None ->
      User_error.raise ~loc
        [ Pp.textf "Module %S was not found in library %S."
            (Module_name.to_string m)
            (Lib_name.to_string @@ Lib.name lib)
        ]
  in
  Memo.parallel_map modules ~f:(fun x -> Memo.return @@ parse x)

let targets ~dir ({ modules; _ } : Coqffi_stanza.t) =
  List.map modules ~f:(target_of ~dir)
