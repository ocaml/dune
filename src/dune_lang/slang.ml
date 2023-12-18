open! Stdune
open Dune_sexp

type t =
  | Nil
  | Literal of String_with_vars.t
  | Form of (Loc.t * form)

and blang = t Blang.Ast.t

and form =
  | Concat of t list
  | When of (blang * t)
  | Cond of
      { cases : (blang * t) list
      ; default : t
      }
  | If of
      { condition : blang
      ; then_ : t
      ; else_ : t
      }
  | Has_undefined_var of t
  | Catch_undefined_var of
      { value : t
      ; fallback : t
      }
  | And_absorb_undefined_var of blang list
  | Or_absorb_undefined_var of blang list
  | Blang of blang

let decode =
  let open Decoder in
  fix (fun decode ->
    let decode_form =
      sum
        ~force_parens:true
        [ ( "concat"
          , let+ x = repeat decode in
            Concat x )
        ; ( "when"
          , let+ condition = Blang.Ast.decode decode
            and+ t = decode in
            When (condition, t) )
        ; ( "cond"
          , let case_or_default =
              (let+ x = pair (Blang.Ast.decode decode) decode in
               `Case x)
              <|> let+ x = decode in
                  `Default x
            in
            let+ terms = repeat (located case_or_default)
            and+ loc = loc in
            match List.destruct_last terms with
            | None ->
              User_error.raise
                ~loc
                [ Pp.text "cond requires at least one argument (its default value)" ]
            | Some (_, (loc, `Case _)) ->
              User_error.raise
                ~loc
                [ Pp.text
                    "The final argument to cond must not have a condition as it's the \
                     default value of the cond expression."
                ]
            | Some (cases, (_, `Default default)) ->
              let cases =
                List.map cases ~f:(fun (loc, term) ->
                  match term with
                  | `Case case -> case
                  | `Default _ ->
                    User_error.raise
                      ~loc
                      [ Pp.text
                          "Conditions are required for each argument to cond except for \
                           the final argument."
                      ])
              in
              Cond { cases; default } )
        ; ( "if"
          , let+ condition = Blang.Ast.decode decode
            and+ then_ = decode
            and+ else_ = decode in
            If { condition; then_; else_ } )
        ; ( "has_undefined_var"
          , let+ x = decode in
            Has_undefined_var x )
        ; ( "catch_undefined_var"
          , let+ value = decode
            and+ fallback = decode in
            Catch_undefined_var { value; fallback } )
        ; ( "and_absorb_undefined_var"
          , let+ x = repeat (Blang.Ast.decode decode) in
            And_absorb_undefined_var x )
        ; ( "or_absorb_undefined_var"
          , let+ x = repeat (Blang.Ast.decode decode) in
            Or_absorb_undefined_var x )
        ]
    in
    located decode_form
    >>| (fun (loc, x) -> Form (loc, x))
    <|> (let+ x = String_with_vars.decode in
         Literal x)
    <|> let+ loc, x = located (Blang.Ast.decode decode) in
        Form (loc, Blang x))
;;

let decode_blang = Blang.Ast.decode decode

let rec encode t =
  let open Encoder in
  match t with
  | Nil ->
    (* there is no syntax for [Nil] so represent it with an expression that
       will always resolve to [Nil] *)
    encode
      (Form
         ( Loc.none
         , When (Blang.Ast.false_, Literal (String_with_vars.make_text Loc.none "")) ))
  | Literal sw -> String_with_vars.encode sw
  | Form (_loc, form) ->
    (match form with
     | Concat ts -> List (string "concat" :: List.map ts ~f:encode)
     | When (condition, t) ->
       List [ string "when"; Blang.Ast.encode encode condition; encode t ]
     | Cond { cases; default } ->
       List
         ((string "cond"
           :: List.map cases ~f:(fun (condition, t) ->
             List [ Blang.Ast.encode encode condition; encode t ]))
          @ [ encode default ])
     | If { condition; then_; else_ } ->
       List [ string "if"; Blang.Ast.encode encode condition; encode then_; encode else_ ]
     | Has_undefined_var t -> List [ string "has_undefined_var"; encode t ]
     | Catch_undefined_var { value; fallback } ->
       List [ string "catch_undefined_var"; encode value; encode fallback ]
     | And_absorb_undefined_var blangs ->
       List
         (string "and_absorb_undefined_var"
          :: List.map blangs ~f:(Blang.Ast.encode encode))
     | Or_absorb_undefined_var blangs ->
       List
         (string "or_absorb_undefined_var" :: List.map blangs ~f:(Blang.Ast.encode encode))
     | Blang b -> Blang.Ast.encode encode b)
;;

let encode_blang = Blang.Ast.encode encode

let rec to_dyn = function
  | Nil -> Dyn.variant "Nil" []
  | Literal sw -> Dyn.variant "Literal" [ String_with_vars.to_dyn sw ]
  | Form (_loc, form) ->
    (match form with
     | Concat ts -> Dyn.variant "Concat" (List.map ts ~f:to_dyn)
     | When (condition, t) ->
       Dyn.variant "When" [ Blang.Ast.to_dyn to_dyn condition; to_dyn t ]
     | Cond { cases; default } ->
       Dyn.variant
         "Cond"
         [ Dyn.record
             [ "cases", Dyn.list (Dyn.pair (Blang.Ast.to_dyn to_dyn) to_dyn) cases
             ; "default", to_dyn default
             ]
         ]
     | If { condition; then_; else_ } ->
       Dyn.variant "If" [ Blang.Ast.to_dyn to_dyn condition; to_dyn then_; to_dyn else_ ]
     | Has_undefined_var t -> Dyn.variant "Has_undefined_var" [ to_dyn t ]
     | Catch_undefined_var { value; fallback } ->
       Dyn.variant "Catch_undefined_var" [ to_dyn value; to_dyn fallback ]
     | And_absorb_undefined_var blangs ->
       Dyn.variant
         "And_absorb_undefined_var"
         (List.map blangs ~f:(Blang.Ast.to_dyn to_dyn))
     | Or_absorb_undefined_var blangs ->
       Dyn.variant
         "Or_absorb_undefined_var"
         (List.map blangs ~f:(Blang.Ast.to_dyn to_dyn))
     | Blang b -> Dyn.variant "Blang" [ Blang.Ast.to_dyn to_dyn b ])
;;

let loc = function
  | Nil -> Loc.none
  | Literal sw -> String_with_vars.loc sw
  | Form (loc, _form) -> loc
;;

let concat ?(loc = Loc.none) ts = Form (loc, Concat ts)
let when_ ?(loc = Loc.none) condition t = Form (loc, When (condition, t))

let if_ ?(loc = Loc.none) condition ~then_ ~else_ =
  Form (loc, If { condition; then_; else_ })
;;

let has_undefined_var ?(loc = Loc.none) t = Form (loc, Has_undefined_var t)

let catch_undefined_var ?(loc = Loc.none) value ~fallback =
  Form (loc, Catch_undefined_var { value; fallback })
;;

let or_absorb_undefined_var ?(loc = Loc.none) blangs =
  Form (loc, Or_absorb_undefined_var blangs)
;;

let and_absorb_undefined_var ?(loc = Loc.none) blangs =
  Form (loc, And_absorb_undefined_var blangs)
;;

let blang ?(loc = Loc.none) t = Form (loc, Blang t)
let pform ?(loc = Loc.none) pform = Literal (String_with_vars.make_pform loc pform)

let text ?(loc = Loc.none) text =
  Literal (String_with_vars.make_text ~quoted:true loc text)
;;

let bool ?(loc = Loc.none) bool = Form (loc, Blang (Blang.Const bool))

let is_nil = function
  | Nil -> true
  | _ -> false
;;

let rec simplify = function
  | Nil -> Nil
  | Literal sw -> Literal sw
  | Form (loc, form) ->
    (match form with
     | Concat with_nil when List.exists with_nil ~f:is_nil ->
       simplify (Form (loc, Concat (List.filter with_nil ~f:(Fun.negate is_nil))))
     | Concat [] -> Literal (String_with_vars.make_text ~quoted:true loc "")
     | Concat [ x ] -> simplify x
     | Concat xs ->
       let simple_terms =
         List.map xs ~f:(function
           | Literal sw ->
             (match String_with_vars.text_only sw with
              | Some text -> Some (`Text text)
              | None ->
                (match String_with_vars.pform_only sw with
                 | Some pform -> Some (`Pform pform)
                 | None -> None))
           | _ -> None)
       in
       if List.for_all simple_terms ~f:Option.is_some
       then (
         (* Each element of the concatenated list is either a string or pform
            so it's trivial to combine them into a single [String_with_vars.t].
         *)
         let parts = List.filter_opt simple_terms in
         let quoted =
           (* only quote strings when not quoting them would be an error *)
           not
             (List.for_all parts ~f:(function
               | `Pform _ -> true
               | `Text s -> Atom.is_valid s))
         in
         let combined_sw = String_with_vars.make ~quoted loc parts in
         Literal combined_sw)
       else Form (loc, Concat (List.map xs ~f:simplify))
     | When (condition, t) ->
       let simplified_condition = simplify_blang condition in
       (match (simplified_condition : blang) with
        | Const true -> t
        | Const false -> Nil
        | _ -> Form (loc, When (simplified_condition, simplify t)))
     | If { condition; then_; else_ } ->
       Form
         ( loc
         , If
             { condition = simplify_blang condition
             ; then_ = simplify then_
             ; else_ = simplify else_
             } )
     | Has_undefined_var t -> Form (loc, Has_undefined_var (simplify t))
     | Catch_undefined_var { value; fallback } ->
       Form
         ( loc
         , Catch_undefined_var { value = simplify value; fallback = simplify fallback } )
     | And_absorb_undefined_var blangs ->
       let blangs : blang list =
         List.concat_map blangs ~f:(fun blang ->
           match simplify_blang blang with
           | Expr (Form (_, And_absorb_undefined_var blangs)) -> blangs
           | blang -> [ blang ])
       in
       Form (loc, And_absorb_undefined_var blangs)
     | Or_absorb_undefined_var blangs ->
       let blangs : blang list =
         List.concat_map blangs ~f:(fun (blang : blang) ->
           match simplify_blang blang with
           | Expr (Form (_, Or_absorb_undefined_var blangs)) -> blangs
           | blang -> [ blang ])
       in
       Form (loc, Or_absorb_undefined_var (List.map blangs ~f:simplify_blang))
     | Blang b -> Form (loc, Blang (simplify_blang b))
     | other -> Form (loc, other))

and simplify_blang = function
  | Const b -> Const b
  | Expr (Literal s) as expr ->
    (match String_with_vars.text_only s with
     | Some "true" -> Const true
     | Some "false" -> Const false
     | _ -> expr)
  | Expr (Form (_, Blang blang)) -> simplify_blang blang
  | Expr s -> Expr (simplify s)
  | Compare (op, lhs, rhs) -> Compare (op, simplify lhs, simplify rhs)
  | Not blang -> Not (simplify_blang blang)
  | And [ b ] -> simplify_blang b
  | And blangs ->
    let blangs =
      List.concat_map blangs ~f:(fun blang ->
        match simplify_blang blang with
        | And xs -> xs
        | blang -> [ blang ])
    in
    And (List.map blangs ~f:simplify_blang)
  | Or [ b ] -> simplify_blang b
  | Or blangs ->
    let blangs =
      List.concat_map blangs ~f:(fun blang ->
        match simplify_blang blang with
        | Or xs -> xs
        | blang -> [ blang ])
    in
    Or (List.map blangs ~f:simplify_blang)
;;
