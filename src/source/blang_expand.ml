open Import
open Memo.O

let rec eval (t : Blang.t) ~dir ~f =
  match t with
  | Const x -> Memo.return x
  | Expr sw ->
    String_expander.Memo.expand sw ~mode:Single ~dir ~f
    >>| (function
     | String "true" -> true
     | String "false" -> false
     | _ ->
       let loc = String_with_vars.loc sw in
       User_error.raise ~loc [ Pp.text "This value must be either true or false" ])
  | And xs ->
      let rec loop = function
      | [] -> Memo.return true
      | x :: xs ->
        let* x = eval ~f ~dir x in
        match x with
        | true -> loop xs
        | false ->
          (* stop evaluating when a false case is reached *)
          Memo.return false
      in loop xs
  | Or xs ->
      let rec loop = function
      | [] -> Memo.return false
      | x :: xs ->
        let* x = eval ~f ~dir x in
        match x with
        | false -> loop xs
        | true ->
          (* stop evaluating when a true case is reached *)
          Memo.return true
      in loop xs
  | Not t -> eval t ~f ~dir >>| not
  | Compare (op, x, y) ->
    let+ x = String_expander.Memo.expand x ~mode:Many ~dir ~f
    and+ y = String_expander.Memo.expand y ~mode:Many ~dir ~f in
    Relop.eval op (Value.L.compare_vals ~dir x y)
;;
