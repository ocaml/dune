open Import
open Dune_sexp

type t =
  | Nil
  | Literal of String_with_vars.t
  | Form of (Loc.t * form)

and blang = t Blang.Ast.t

and form =
  | Concat of t list
  | When of (blang * t)
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

let rec equal a b =
  let form_equal a b =
    match a, b with
    | Concat a, Concat b -> List.equal equal a b
    | When a, When b -> Tuple.T2.equal blang_equal equal a b
    | ( If { condition = a_condition; then_ = a_then_; else_ = a_else_ }
      , If { condition = b_condition; then_ = b_then_; else_ = b_else_ } ) ->
      blang_equal a_condition b_condition
      && equal a_then_ b_then_
      && equal a_else_ b_else_
    | Has_undefined_var a, Has_undefined_var b -> equal a b
    | ( Catch_undefined_var { value = a_value; fallback = a_fallback }
      , Catch_undefined_var { value = b_value; fallback = b_fallback } ) ->
      equal a_value b_value && equal a_fallback b_fallback
    | And_absorb_undefined_var a, And_absorb_undefined_var b
    | Or_absorb_undefined_var a, Or_absorb_undefined_var b -> List.equal blang_equal a b
    | Blang a, Blang b -> blang_equal a b
    | _, _ -> false
  in
  match a, b with
  | Nil, Nil -> true
  | Literal a, Literal b -> String_with_vars.equal a b
  | Form a, Form b -> Tuple.T2.equal Loc.equal form_equal a b
  | _, _ -> false

and blang_equal a b = Blang.Ast.equal equal a b

let rec remove_locs t =
  let form_remove_locs = function
    | Concat ts -> Concat (List.map ts ~f:remove_locs)
    | When (blang, t) -> When (blang_remove_locs blang, remove_locs t)
    | If { condition; then_; else_ } ->
      If
        { condition = blang_remove_locs condition
        ; then_ = remove_locs then_
        ; else_ = remove_locs else_
        }
    | Has_undefined_var t -> Has_undefined_var (remove_locs t)
    | Catch_undefined_var { value; fallback } ->
      Catch_undefined_var { value = remove_locs value; fallback = remove_locs fallback }
    | And_absorb_undefined_var blangs ->
      And_absorb_undefined_var (List.map blangs ~f:blang_remove_locs)
    | Or_absorb_undefined_var blangs ->
      Or_absorb_undefined_var (List.map blangs ~f:blang_remove_locs)
    | Blang blang -> Blang (blang_remove_locs blang)
  in
  match t with
  | Nil -> Nil
  | Literal sw -> Literal (String_with_vars.remove_locs sw)
  | Form (_loc, form) -> Form (Loc.none, form_remove_locs form)

and blang_remove_locs blang = Blang.Ast.map_string ~f:remove_locs blang

let decode_literal =
  let open Decoder in
  let+ x = String_with_vars.decode in
  Literal x
;;

let decode =
  let open Decoder in
  fix (fun decode ->
    let decode_blang = Blang.Ast.decode ~override_decode_bare_literal:None decode in
    let decode_form =
      sum
        ~force_parens:true
        [ ( "concat"
          , let+ x = repeat decode in
            Concat x )
        ; ( "when"
          , let+ condition = decode_blang
            and+ t = decode in
            When (condition, t) )
        ; ( "if"
          , let+ condition = decode_blang
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
          , let+ x = repeat decode_blang in
            And_absorb_undefined_var x )
        ; ( "or_absorb_undefined_var"
          , let+ x = repeat decode_blang in
            Or_absorb_undefined_var x )
        ]
    in
    located decode_form
    >>| (fun (loc, x) -> Form (loc, x))
    <|> decode_literal
    <|>
    (* The decoders for the blang and slang DSLs are mutually recursive
       since blang expressions can appear as the conditions in slang
       expressions and slang expressions can be compared in blang
       expressions, and also produce blang literals. When encountering
       a form which is not valid syntax for slang nor blang each
       decoder will attempt to invoke the other leading to infinite
       recursion. to prevent this, only attempt to parse [literal _] values
       when the blang parser parses literals.*)
    let+ loc, x =
      located
        (Blang.Ast.decode ~override_decode_bare_literal:(Some decode_literal) decode)
    in
    Form (loc, Blang x))
;;

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

let rec to_dyn = function
  | Nil -> Dyn.variant "Nil" []
  | Literal sw -> Dyn.variant "Literal" [ String_with_vars.to_dyn sw ]
  | Form (_loc, form) ->
    (match form with
     | Concat ts -> Dyn.variant "Concat" (List.map ts ~f:to_dyn)
     | When (condition, t) -> Dyn.variant "When" [ blang_to_dyn condition; to_dyn t ]
     | If { condition; then_; else_ } ->
       Dyn.variant "If" [ Blang.Ast.to_dyn to_dyn condition; to_dyn then_; to_dyn else_ ]
     | Has_undefined_var t -> Dyn.variant "Has_undefined_var" [ to_dyn t ]
     | Catch_undefined_var { value; fallback } ->
       Dyn.variant "Catch_undefined_var" [ to_dyn value; to_dyn fallback ]
     | And_absorb_undefined_var blangs ->
       Dyn.variant "And_absorb_undefined_var" (List.map blangs ~f:blang_to_dyn)
     | Or_absorb_undefined_var blangs ->
       Dyn.variant "Or_absorb_undefined_var" (List.map blangs ~f:blang_to_dyn)
     | Blang b -> Dyn.variant "Blang" [ blang_to_dyn b ])

and blang_to_dyn blang = Blang.Ast.to_dyn to_dyn blang

let loc = function
  | Nil -> Loc.none
  | Literal sw -> String_with_vars.loc sw
  | Form (loc, _form) -> loc
;;

let blang_map = Blang.Ast.map_string

let rec map_loc ~f = function
  | Nil -> Nil
  | Literal sw -> Literal (String_with_vars.map_loc ~f sw)
  | Form (loc, form) ->
    let loc = f loc in
    let form = map_form_loc ~f form in
    Form (loc, form)

and map_form_loc ~f = function
  | Concat ts -> Concat (List.map ~f:(map_loc ~f) ts)
  | When (blang, t) ->
    let blang = blang_map blang ~f:(map_loc ~f) in
    let t = map_loc ~f t in
    When (blang, t)
  | If { condition; then_; else_ } ->
    let condition = blang_map condition ~f:(map_loc ~f) in
    let then_ = map_loc ~f then_ in
    let else_ = map_loc ~f else_ in
    If { condition; then_; else_ }
  | Has_undefined_var t ->
    let t = map_loc ~f t in
    Has_undefined_var t
  | Catch_undefined_var { value; fallback } ->
    let value = map_loc ~f value in
    let fallback = map_loc ~f fallback in
    Catch_undefined_var { value; fallback }
  | And_absorb_undefined_var blangs ->
    let blangs = List.map blangs ~f:(blang_map ~f:(map_loc ~f)) in
    And_absorb_undefined_var blangs
  | Or_absorb_undefined_var blangs ->
    let blangs = List.map blangs ~f:(blang_map ~f:(map_loc ~f)) in
    Or_absorb_undefined_var blangs
  | Blang blang ->
    let blang = blang_map blang ~f:(map_loc ~f) in
    Blang blang
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
     | Blang b -> Form (loc, Blang (simplify_blang b)))

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

module Blang = struct
  type t = blang

  let equal = blang_equal
  let to_dyn = blang_to_dyn
  let remove_locs = blang_remove_locs
  let map_loc ~f = blang_map ~f:(map_loc ~f)
  let true_ = Blang.Const true
  let false_ = Blang.Const false
  let decode = Blang.Ast.decode ~override_decode_bare_literal:None decode
  let encode = Blang.Ast.encode encode
end
