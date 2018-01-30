open! Import

type var_syntax = Parens | Braces

type item =
  | Text of string
  | Var of var_syntax * string

type t =
  { items : item list
  ; loc : Loc.t
  ; quoted : bool }

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

(* Remark: Consecutive [Text] items are concatenated. *)
let rec of_tokens : Token.t list -> item list = function
  | [] -> []
  | Open a :: String s :: Close b :: rest when a = b ->
    Var (a, s) :: of_tokens rest
  | token :: rest ->
    let s = Token.to_string token in
    match of_tokens rest with
    | Text s' :: l -> Text (s ^ s') :: l
    | l -> Text s :: l

let items_of_string s = of_tokens (Token.tokenise s)

let unquoted_var t =
  match t.quoted, t.items with
  | true, [Var(_, s)] -> Some s
  | _ -> None

let t : Sexp.Of_sexp.ast -> t = function
  | Atom(loc, s) -> { items = items_of_string s;  loc;  quoted = false }
  | Quoted_string (loc, s) ->
     { items = items_of_string s;  loc;  quoted = true }
  | List _ as sexp -> Sexp.Of_sexp.of_sexp_error sexp "Atom expected"

let loc t = t.loc

let virt ?(quoted=true) pos s =
  { items = items_of_string s;  loc = Loc.of_pos pos;  quoted }
let virt_var ?(quoted=true) pos s =
  { items = [Var (Braces, s)];  loc = Loc.of_pos pos;  quoted }
let virt_text pos s =
  { items = [Text s];  loc = Loc.of_pos pos;  quoted = true }

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

let iter t ~f = List.iter t.items ~f:(function
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
        | _  ->
           Inr { t with items = List.rev (commit_text acc_text acc) }
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
