open! Import

type var_syntax = Parens | Braces

type item =
  | Text of string
  | Var of var_syntax * string

(* A single unquoted variable is encoded as the list [Var v].  A
   quoted variable is encoded as [Var v; Text ""]. *)
type t =
  { items : item list
  ; loc   : Loc.t
  }

module Token = struct
  type t =
    | String of string
    | Open   of var_syntax
    | Close  of var_syntax

  let tokenise s =
    let len = String.length s in
    let sub i j = String.sub s ~pos:i ~len:(j - i) in
    let cons_str i j acc = if i = j then acc else String (sub i j) :: acc in
    let rec loop i j =
      if j = len
      then cons_str i j []
      else
        match s.[j] with
        | '}' -> cons_str i j (Close Braces :: loop (j + 1) (j + 1))
        | ')' -> cons_str i j (Close Parens :: loop (j + 1) (j + 1))
        | '$' when j + 1 < len -> begin
            match s.[j + 1] with
            | '{' -> cons_str i j (Open Braces :: loop (j + 2) (j + 2))
            | '(' -> cons_str i j (Open Parens :: loop (j + 2) (j + 2))
            | _   -> loop i (j + 1)
          end
        | _ -> loop i (j + 1)
    in
    loop 0 0

  let to_string = function
    | String s     -> s
    | Open  Braces -> "${"
    | Open  Parens -> "$("
    | Close Braces -> "}"
    | Close Parens -> ")"
end

let rec of_tokens : Token.t list -> item list = function
  | [] -> []
  | Open a :: String s :: Close b :: rest when a = b ->
    Var (a, s) :: of_tokens rest
  | token :: rest ->
    let s = Token.to_string token in
    match of_tokens rest with
    | Text s' :: l -> Text (s ^ s') :: l
    | l -> Text s :: l

let of_string ~loc s =
  { items = of_tokens (Token.tokenise s)
  ; loc
  }

let t sexp = of_string ~loc:(Sexp.Ast.loc sexp) (Sexp.Of_sexp.string sexp)

let loc t = t.loc

let virt pos s = of_string ~loc:(Loc.of_pos pos) s
let virt_var  pos s = { loc = Loc.of_pos pos; items = [Var (Braces, s)] }
let virt_quoted_var pos s = { loc = Loc.of_pos pos;
                              items = [Var (Braces, s); Text ""] }
let virt_text pos s = { loc = Loc.of_pos pos; items = [Text s] }

let unquoted_var t =
  match t.items with
  | [Var (_, s)] -> Some s
  | _ -> None

let sexp_of_var_syntax = function
  | Parens -> Sexp.Atom "parens"
  | Braces -> Sexp.Atom "braces"

let sexp_of_item =
  let open Sexp in function
    | Text s -> List [Atom "text" ; Atom s]
    | Var (vs, s) -> List [sexp_of_var_syntax vs ; Atom s]

let sexp_of_t t = Sexp.To_sexp.list sexp_of_item t.items


let fold t ~init ~f =
  List.fold_left t.items ~init ~f:(fun acc item ->
    match item with
    | Text _ -> acc
    | Var (_, v) -> f acc t.loc v)

let iter t ~f =
  List.iter t.items ~f:(function
    | Text _ -> ()
    | Var (_, v) -> f t.loc v)

let vars t = fold t ~init:String_set.empty ~f:(fun acc _ x -> String_set.add x acc)

let string_of_var syntax v =
  match syntax with
  | Parens -> sprintf "$(%s)" v
  | Braces -> sprintf "${%s}" v

let expand t ~f =
  List.map t.items ~f:(function
    | Text s -> s
    | Var (syntax, v) ->
      match f t.loc v with
      | Some x -> x
      | None -> string_of_var syntax v)
  |> String.concat ~sep:""

let concat_rev = function
  | [] -> ""
  | [s] -> s
  | l -> String.concat (List.rev l) ~sep:""

let partial_expand t ~f =
  let commit_text acc_text acc =
    let s = concat_rev acc_text in
    if s = "" then acc else Text s :: acc
  in
  let rec loop acc_text acc items =
    match items with
    | [] -> begin
        match acc with
        | [] -> Inl (concat_rev acc_text)
        | _  -> Inr { t with items = List.rev (commit_text acc_text acc) }
      end
    | Text s :: items -> loop (s :: acc_text) acc items
    | Var (_, v) as it :: items ->
      match f t.loc v with
      | None -> loop [] (it :: commit_text acc_text acc) items
      | Some s -> loop (s :: acc_text) acc items
  in
  loop [] [] t.items

let to_string t =
  match t.items with
  (* [to_string is only called from action.ml, always on [t]s of this form *)
  | [Var (syntax, v)] -> string_of_var syntax v
  | items ->
    List.map items ~f:(function
      | Text s -> s
      | Var (syntax, v) -> string_of_var syntax v)
    |> String.concat ~sep:""
