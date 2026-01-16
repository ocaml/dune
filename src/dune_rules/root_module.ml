open Import
open Memo.O

let entry_module_names sctx lib ~for_ =
  match Lib_info.entry_modules (Lib.info lib) with
  | External d -> Resolve.Memo.of_result d
  | Local ->
    let+ modules =
      Dir_contents.modules_of_local_lib sctx (Lib.Local.of_lib_exn lib) ~for_
    in
    modules |> Modules.entry_modules |> List.map ~f:Module.name |> Resolve.return
;;

let entries sctx ~requires_compile ~for_ =
  let open Action_builder.O in
  let* requires = Resolve.Memo.read requires_compile in
  let* l =
    Action_builder.List.map requires ~f:(fun lib ->
      Action_builder.of_memo (entry_module_names sctx lib ~for_) >>= Resolve.read)
  in
  Action_builder.return (List.concat l)
;;
