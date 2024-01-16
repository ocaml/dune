open! Stdune
open! Import

let eval (cond : Dune_lang.Cond.t) ~dir ~f =
  let open Memo.O in
  Memo.List.find_map cond.conditions ~f:(fun (blang, sw) ->
    let* condition = Blang_expand.eval blang ~dir ~f in
    if condition
    then String_expander.Memo.expand sw ~mode:Single ~dir ~f >>| Option.some
    else Memo.return None)
;;
