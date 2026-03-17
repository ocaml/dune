open Import
open Memo.O

let eval ~dir ~f =
  let rec eval t =
    match (t : Blang.t) with
    | Const x -> Memo.return x
    | Expr sw ->
      String_expander.Memo.expand sw ~mode:Single ~dir ~f
      >>| (function
       | String "true" -> true
       | String "false" -> false
       | _ ->
         let loc = String_with_vars.loc sw in
         User_error.raise ~loc [ Pp.text "This value must be either true or false" ])
    | And xs -> Memo.List.map xs ~f:eval >>| List.for_all ~f:Fun.id
    | Or xs -> Memo.List.map xs ~f:eval >>| List.exists ~f:Fun.id
    | Not t -> eval t >>| not
    | Compare (op, x, y) ->
      let+ x = String_expander.Memo.expand x ~mode:Many ~dir ~f
      and+ y = String_expander.Memo.expand y ~mode:Many ~dir ~f in
      Relop.eval op (Value.L.compare_vals ~dir x y)
  in
  fun t -> eval t
[@@inline]
;;

let eval t ~dir ~f = eval ~dir ~f t
