open Import

let expand t ~f =
  let open Memo.O in
  match t with
  | Rule_mode.Standard -> Memo.return Rule.Mode.Standard
  | Fallback -> Memo.return Rule.Mode.Fallback
  | Ignore_source_files -> Memo.return Rule.Mode.Ignore_source_files
  | Promote { lifetime; into; only } ->
    let+ into =
      Memo.Option.map into ~f:(fun { loc; dir } ->
        let+ dir = f dir in
        { Rule.Promote.Into.loc; dir })
    in
    Rule.Mode.Promote { lifetime; only; into }
;;

let expand_path t ~expander ~dir =
  expand t ~f:(fun into_dir ->
    let open Memo.O in
    let+ path, _ =
      Action_builder.evaluate_and_collect_facts (Expander.expand_path expander into_dir)
    in
    Path.reach ~from:(Path.build dir) path)
;;

let expand_str t ~expander =
  expand t ~f:(fun into_dir ->
    let open Memo.O in
    let+ path, _ =
      Action_builder.evaluate_and_collect_facts (Expander.expand_str expander into_dir)
    in
    path)
;;
