let need_quoting s =
  let len = String.length s in
  len = 0 ||
  let rec loop i =
    if i = len then
      false
    else
      match s.[i] with
      | ' ' | '\"' | '(' | ')' | '{' | '}' | ';' | '#' -> true
      | _ -> loop (i + 1)
  in
  loop 0

let quote_for_shell s =
  if need_quoting s then
    Filename.quote s
  else
    s

let print_to_console = Console.print
let sprintf = Printf.sprintf
let ksprintf = Printf.ksprintf
