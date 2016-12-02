open! Import

type var_syntax = Parens | Braces

type item =
  | Text of string
  | Var of var_syntax * string

type t = item list

let syntax_of_opening = function
  | '{' -> Braces
  | '(' -> Parens
  | _   -> assert false

let of_string s =
  let len = String.length s in
  let sub i j = String.sub s ~pos:i ~len:(j - i) in
  let cons_text i j acc = if i = j then acc else Text (sub i j) :: acc in
  let rec loop i j =
    if j = len then
      cons_text i j []
    else
      match s.[j] with
      | '$' ->  begin
          match
            match s.[j + 1] with
            | '{' -> String.index_from s (j + 2) '}'
            | '(' -> String.index_from s (j + 2) ')'
            | _   -> raise Not_found
          with
          | exception Not_found -> loop i (j + 1)
          | var_end ->
            let var = sub (j + 2) var_end in
            let syntax = syntax_of_opening s.[j + 1] in
            cons_text i j (Var (syntax, var) :: loop (var_end + 1) (var_end + 1))
        end
      | _ -> loop i (j + 1)
  in
  loop 0 0

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

