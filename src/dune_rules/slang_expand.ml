open! Stdune
open Import
module Package_variable = Dune_pkg.Package_variable

type expander =
  String_with_vars.t
  -> dir:Path.t
  -> (Value.t list, [ `Undefined_pkg_var of Package_variable.Name.t ]) result Memo.t

type error =
  | Undefined_pkg_var of
      { literal : String_with_vars.t
      ; variable_name : Package_variable.Name.t
      }

let rec eval_rec (t : Slang.t) ~dir ~f =
  let open Memo.O in
  match t with
  | Nil -> Memo.return (Ok [])
  | Literal sw ->
    f sw ~dir
    >>| Result.map_error ~f:(function `Undefined_pkg_var variable_name ->
      Undefined_pkg_var { literal = sw; variable_name })
  | Form (_loc, form) ->
    (match form with
     | Concat xs ->
       let+ evaluated = Memo.List.map xs ~f:(eval_rec ~dir ~f) >>| Result.List.all in
       Result.map evaluated ~f:(fun evaluated ->
         let without_nils = List.concat evaluated in
         (* XXX if any of the arguments were [Value.Path]s this will convert them
            into [Value.String]s *)
         [ Value.String
             (List.map ~f:(Value.to_string ~dir) without_nils |> String.concat ~sep:"")
         ])
     | When (condition, t) ->
       let* condition = eval_blang_rec condition ~dir ~f in
       (match condition with
        | Error _ as e -> Memo.return e
        | Ok false -> Memo.return (Ok [])
        | Ok true -> eval_rec t ~dir ~f)
     | If { condition; then_; else_ } ->
       let* condition = eval_blang_rec condition ~dir ~f in
       (match condition with
        | Error _ as e -> Memo.return e
        | Ok true -> eval_rec then_ ~dir ~f
        | Ok false -> eval_rec else_ ~dir ~f)
     | Has_undefined_var t ->
       let+ result = eval_rec t ~dir ~f in
       Ok
         (match result with
          | Ok _ -> [ Value.false_ ]
          | Error (Undefined_pkg_var _) -> [ Value.true_ ])
     | Catch_undefined_var { value; fallback } ->
       let* value = eval_rec value ~dir ~f in
       (match value with
        | Ok value -> Memo.return @@ Ok value
        | Error (Undefined_pkg_var _) -> eval_rec fallback ~dir ~f)
     | And_absorb_undefined_var blangs ->
       let rec loop acc = function
         | [] -> Memo.return acc
         | x :: xs ->
           let* x = eval_blang_rec x ~dir ~f in
           (match x with
            | Error _ as e ->
              (* Propagate the first error rather than the last *)
              if Result.is_ok acc then loop e xs else loop acc xs
            | Ok true -> loop acc xs
            | Ok false -> Memo.return (Ok [ Value.false_ ]))
       in
       loop (Ok [ Value.true_ ]) blangs
     | Or_absorb_undefined_var blangs ->
       let rec loop acc = function
         | [] -> Memo.return acc
         | x :: xs ->
           let* x = eval_blang_rec x ~dir ~f in
           (match x with
            | Error _ as e ->
              (* Propagate the first error rather than the last *)
              if Result.is_ok acc then loop e xs else loop acc xs
            | Ok false -> loop acc xs
            | Ok true -> Memo.return (Ok [ Value.true_ ]))
       in
       loop (Ok [ Value.false_ ]) blangs
     | Blang b ->
       let+ result = eval_blang_rec b ~dir ~f in
       Result.map result ~f:(function
         | true -> [ Value.true_ ]
         | false -> [ Value.false_ ]))

and eval_to_bool (t : Slang.t) ~dir ~f =
  let open Memo.O in
  let+ result = eval_rec t ~dir ~f in
  Result.map result ~f:(function
    | [ x ] when Value.(equal true_ x) -> true
    | [ x ] when Value.(equal false_ x) -> false
    | [ Value.String other ] ->
      User_error.raise
        ~loc:(Slang.loc t)
        [ Pp.textf
            "This expression is used as a condition and so must evaluate to either \
             \"true\" or \"false\", however it evaluated to %S."
            other
        ]
    | [ (Dir path | Path path) ] ->
      User_error.raise
        ~loc:(Slang.loc t)
        [ Pp.textf
            "This expression is used as a condition and so must evaluate to a string \
             whose value is either \"true\" or \"false\", however it evaluated to the \
             path: %s"
            (Path.to_string path)
        ]
    | [] ->
      User_error.raise
        ~loc:(Slang.loc t)
        [ Pp.text
            "This expression is used as a condition and so must evaluate to a single \
             string whose value is either \"true\" or \"false\", however it evaluated to \
             Nil (ie. zero strings)"
        ]
    | _ :: _ :: _ as multiple_values ->
      User_error.raise
        ~loc:(Slang.loc t)
        [ Pp.textf
            "This expression is used as a condition and so must evaluate to a single \
             string whose value is either \"true\" or \"false\", however it evaluated to \
             multiple strings: %s"
            (List.map multiple_values ~f:(Value.to_string ~dir) |> String.enumerate_and)
        ])

and eval_blang_rec (t : Slang.blang) ~dir ~f =
  let open Memo.O in
  match t with
  | Const x -> Memo.return (Ok x)
  | Expr s -> eval_to_bool s ~dir ~f
  | And xs ->
    let rec loop = function
      | [] -> Memo.return (Ok true)
      | x :: xs ->
        let* x = eval_blang_rec x ~dir ~f in
        (match x with
         | Error _ as e -> Memo.return e
         | Ok true -> loop xs
         | Ok false ->
           (* stop evaluating when a false case is reached *)
           Memo.return (Ok false))
    in
    loop xs
  | Or xs ->
    let rec loop = function
      | [] -> Memo.return (Ok false)
      | x :: xs ->
        let* x = eval_blang_rec x ~dir ~f in
        (match x with
         | Error _ as e -> Memo.return e
         | Ok false -> loop xs
         | Ok true ->
           (* stop evaluating when a true case is reached *)
           Memo.return (Ok true))
    in
    loop xs
  | Not blang ->
    let+ result = eval_blang_rec blang ~dir ~f in
    Result.map result ~f:not
  | Compare (op, x, y) ->
    let+ x = eval_rec x ~dir ~f
    and+ y = eval_rec y ~dir ~f in
    Result.bind x ~f:(fun x ->
      Result.map y ~f:(fun y -> Blang.Op.eval op (Value.L.compare_vals ~dir x y)))
;;

let eval t ~dir ~f =
  let open Memo.O in
  let+ result = eval_rec t ~dir ~f in
  match result with
  | Ok value -> value
  | Error (Undefined_pkg_var { literal; variable_name }) ->
    User_error.raise
      ~loc:(String_with_vars.loc literal)
      [ Pp.textf
          "Undefined package variable %S"
          (Package_variable.Name.to_string variable_name)
      ]
;;

let eval_multi_located ts ~dir ~f =
  let open Memo.O in
  Memo.List.concat_map ts ~f:(fun t ->
    let+ values = eval t ~dir ~f in
    List.map values ~f:(fun value -> Slang.loc t, value))
;;

let eval_blang blang ~dir ~f =
  let open Memo.O in
  let+ result = eval_blang_rec blang ~dir ~f in
  match result with
  | Ok value -> value
  | Error (Undefined_pkg_var { literal; variable_name }) ->
    User_error.raise
      ~loc:(String_with_vars.loc literal)
      [ Pp.textf
          "Undefined package variable %S"
          (Package_variable.Name.to_string variable_name)
      ]
;;
