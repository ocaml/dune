open! Stdune
open! Import

let eval (cond : Dune_lang.Cond.t) ~dir ~f =
  let open Memo.O in
  let expand_sw = String_expander.Memo.expand ~mode:Single ~dir ~f in
  let* from_case =
    Memo.List.find_map cond.cases ~f:(fun (blang, sw) ->
      let* condition = Blang_expand.eval blang ~dir ~f in
      if condition then expand_sw sw >>| Option.some else Memo.return None)
  in
  if Option.is_some from_case
  then Memo.return from_case
  else (
    match cond.default with
    | None -> Memo.return None
    | Some default -> expand_sw default >>| Option.some)
;;
