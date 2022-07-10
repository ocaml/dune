open! Stdune
include Deps

let rec read_buffer_aux t buf =
  match Lexer.parse_coq t buf with
  | t -> read_buffer_aux t buf
  | exception Lexer.End_of_file -> t

let read_buffer buf = read_buffer_aux Deps.empty buf

let lexbuf buf = read_buffer buf |> Deps.sort_uniq

let to_csexp t = Deps.to_sexp t

let of_csexp t = Deps.of_sexp t
