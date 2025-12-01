open Import
include Rule_mode

let expand_path t ~expander ~dir =
  let open Memo.O in
  match t with
  | Standard -> Memo.return Rule.Mode.Standard
  | Fallback -> Memo.return Rule.Mode.Fallback
  | Promote { lifetime; into; only } ->
    let+ into =
      match into with
      | None -> Memo.return None
      | Some { loc; dir = into_dir } ->
        let+ path, _ =
          Action_builder.evaluate_and_collect_facts
            (Expander.expand_path expander into_dir)
        in
        let dir = Path.reach ~from:(Path.build dir) path in
        Some { Rule.Promote.Into.loc; dir }
    in
    Rule.Mode.Promote { lifetime; only; into }
  | Ignore_source_files -> Memo.return Rule.Mode.Ignore_source_files
;;

let expand_str t ~expander =
  let open Memo.O in
  match t with
  | Standard -> Memo.return Rule.Mode.Standard
  | Fallback -> Memo.return Rule.Mode.Fallback
  | Promote { lifetime; into; only } ->
    let+ into =
      match into with
      | None -> Memo.return None
      | Some { loc; dir = into_dir } ->
        let+ path, _ =
          Action_builder.evaluate_and_collect_facts
            (Expander.expand_str expander into_dir)
        in
        Some { Rule.Promote.Into.loc; dir = path }
    in
    Rule.Mode.Promote { lifetime; only; into }
  | Ignore_source_files -> Memo.return Rule.Mode.Ignore_source_files
;;
