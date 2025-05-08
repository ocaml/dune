open Import

let lock_dir dev_tool =
  let open Memo.O in
  let* lock_dir_enabled = Dune_rules.Lock_dir.enabled in
  match lock_dir_enabled with
  | false -> Memo.return ()
  | true ->
    let lock_dir_path = Dune_rules.Lock_dir.dev_tool_lock_dir_path dev_tool in
    Build_system.build_dir lock_dir_path
;;
