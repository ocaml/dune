module List = Stdlib.ListLabels
module String = Stdlib.StringLabels
open String

external index_from : string -> int -> char -> int = "dune_string_index_from" [@@noalloc]

let split s ~on =
  let len = length s in
  let rec loop i =
    let separator = if i = len then -1 else index_from s i on in
    match separator with
    | -1 -> [ sub s ~pos:i ~len:(len - i) ]
    | j -> sub s ~pos:i ~len:(j - i) :: loop (j + 1)
  in
  loop 0
;;

let split_lines s =
  let len = length s in
  let rec loop acc i =
    let newline = if i = len then -1 else index_from s i '\n' in
    match newline with
    | -1 ->
      let acc =
        if i = len || (i + 1 = len && String.unsafe_get s i = '\r')
        then acc
        else sub s ~pos:i ~len:(len - i) :: acc
      in
      List.rev acc
    | j ->
      let line_len =
        if j > i && String.unsafe_get s (j - 1) = '\r' then j - i - 1 else j - i
      in
      loop (sub s ~pos:i ~len:line_len :: acc) (j + 1)
  in
  loop [] 0
;;
