open Import

type t =
  { name    : string
  ; entries : entry list
  }

and entry =
  | Comment of string
  | Var     of string * predicate list * action * string
  | Package of t

and action = Set | Add

and predicate =
  | P of string
  | A of string

module Parse = struct
  let error = lex_error

  let next = Meta_lexer.token

  let package_name lb =
    match next lb with
    | String s ->
      if String.contains s '.' then
        error lb "'.' not allowed in sub-package names";
      s
    | _ -> error lb "package name expected"

  let string lb =
    match lb with
    | String s -> s
    | _ -> error lb "string expected"

  let lparen lb =
    match next lb with
    | Lparen -> ()
    | _ -> error lb "'(' expected"

  let action lb =
    match next lb with
    | Equal      -> Set
    | Plus_equal -> Add
    | _          -> error lb "'=' or '+=' expected"

  let comma lb =
    match next lb with
    | Comma -> ()
    | _     -> error lb "',' expected"

  let rec predicates_and_action lb acc =
    match next lb with
    | Rparen -> (List.rev acc, action lb)
    | Name n -> comma lb; predicates_and_action lb (P n :: acc)
    | Minus  ->
      let n =
        match next lb with
        | Name p -> p
        | _      -> error lb "name expected"
      in
      comma lb;
      predicates_and_action lb (A n :: acc)
    | _          -> error lb "name, '-' or ')' expected"

  let rec entries lb depth acc =
    match next lb with
    | Rparen ->
      if depth > 0 then
        List.rev acc
      else
        error lb "closing parenthesis without matching opening one"
    | Name "package" ->
      let name = package_name lb in
      lparen lb;
      let entries = entries lb (depth + 1) [] in
      entries lb depth (Package { name; entries } :: acc)
    | Name var ->
      let preds, action =
        match next lb with
        | Equal      -> ([], Set)
        | Plus_equal -> ([], Add)
        | Lparen     -> predicates_and_action lb []
        | _          -> error lb "'=', '+=' or '(' expected"
      in
      let value = string lb in
      Var (var, preds, action, value)
    | _ ->
      error lb "'package' or variable name expected"
end

let load fn =
  with_lexbuf_from_file fn ~f:(fun lb ->
      Parse.entries lb 0 [])
