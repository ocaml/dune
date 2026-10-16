open Import
open Memo.O

let rec eval (t : Blang.t) ~dir ~f =
  match t with
  | Const x -> Memo.return x
  | Expr sw ->
    String_expander.Memo.expand sw ~mode:Single ~dir ~f
    >>| fun s ->
    (match
       match s with
       | String s -> Bool.of_string s
       | _ -> None
     with
     | Some s -> s
     | None ->
       let loc = String_with_vars.loc sw in
       User_error.raise ~loc [ Pp.text "This value must be either true or false" ])
  | And xs -> Memo.List.for_all xs ~f:(fun x -> eval ~f ~dir x)
  | Or xs -> Memo.List.exists xs ~f:(fun x -> eval ~f ~dir x)
  | Not t -> eval t ~f ~dir >>| not
  | Compare (op, x, y) ->
    let+ x = String_expander.Memo.expand x ~mode:Many ~dir ~f
    and+ y = String_expander.Memo.expand y ~mode:Many ~dir ~f in
    Relop.eval op (Value.L.compare_vals ~dir x y)
;;
