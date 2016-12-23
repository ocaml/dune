open! Import

type var_syntax = Parens | Braces

type item =
  | Text of string
  | Var of var_syntax * string

type t = item list

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
        | '$' -> begin
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

let rec of_tokens : Token.t list -> t = function
  | [] -> []
  | Open a :: String s :: Close b :: rest when a = b ->
    Var (a, s) :: of_tokens rest
  | token :: rest ->
    let s = Token.to_string token in
    match of_tokens rest with
    | Text s' :: l -> Text (s ^ s') :: l
    | l -> Text s :: l

let of_string s = of_tokens (Token.tokenise s)

let t sexp = of_string (Sexp.Of_sexp.string sexp)

let fold t ~init ~f =
  List.fold_left t ~init ~f:(fun acc item ->
    match item with
    | Text _ -> acc
    | Var (_, v) -> f acc v)

let vars t = fold t ~init:String_set.empty ~f:(fun acc x -> String_set.add x acc)

let expand t ~f =
  List.map t ~f:(function
    | Text s -> s
    | Var (syntax, v) ->
      match f v with
      | Some x -> x
      | None ->
        match syntax with
        | Parens -> sprintf "$(%s)" v
        | Braces -> sprintf "${%s}" v)
  |> String.concat ~sep:""

module type Container = sig
  type 'a t
  val t : (Sexp.t -> 'a) -> Sexp.t -> 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t
  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
end

module Lift(M : Container) = struct
  type nonrec t = t M.t
  let t sexp = M.t t sexp

  let fold t ~init ~f =
    M.fold t ~init ~f:(fun acc x -> fold x ~init:acc ~f)

  let expand t ~f = M.map t ~f:(expand ~f)
end

