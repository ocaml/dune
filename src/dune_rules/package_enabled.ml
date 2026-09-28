open Import
open Memo.O

let eval package =
  match Package.enabled_if package with
  | None -> Memo.return true
  | Some expr ->
    Blang_expand.eval expr ~dir:Path.root ~f:(fun ~source:_ pform ->
      match pform with
      | Var (Os v) -> Lock_dir.Sys_vars.(os_values poll v)
      | Var Architecture ->
        let+ arch = Memo.Lazy.force Lock_dir.Sys_vars.poll.arch in
        [ Value.String (Option.value ~default:"" arch) ]
      | pform ->
        Code_error.raise
          "Package_enabled.eval: variable not allowed"
          [ "variable", Pform.to_dyn pform ])
;;
