open Import
open Memo.O

let all =
  Memo.lazy_ ~name:"context-db"
  @@ fun () ->
  let+ workspace = Workspace.workspace () in
  let contexts =
    List.concat_map workspace.contexts ~f:(fun (context : Workspace.Context.t) ->
      let native, targets =
        match context with
        | Default default -> default.base.name, default.base.targets
        | Opam opam -> opam.base.name, opam.base.targets
      in
      let targets =
        List.filter_map targets ~f:(function
          | Native -> None
          | Named toolchain ->
            let name = Context_name.target native ~toolchain in
            Some (name, `Target (context, toolchain)))
      in
      (native, `Native context) :: targets)
  in
  Context_name.Map.of_list_exn contexts
;;

let create_db ?cutoff ~name f =
  let cutoff = Option.map cutoff ~f:(fun equal -> Context_name.Map.equal ~equal) in
  let map =
    Memo.lazy_ ~name ?cutoff (fun () ->
      let+ map = Memo.Lazy.force all in
      Context_name.Map.map map ~f)
  in
  Staged.stage (fun context ->
    let+ map = Memo.Lazy.force map in
    match Context_name.Map.find map context with
    | Some v -> v
    | None ->
      Code_error.raise "invalid context" [ "context", Context_name.to_dyn context ])
;;

let profile =
  create_db ~cutoff:Profile.equal ~name:"profile" (function
    | `Native ctx | `Target (ctx, _) -> (Workspace.Context.base ctx).profile)
  |> Staged.unstage
;;
