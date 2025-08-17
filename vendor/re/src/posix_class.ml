module Re = Core

let of_name = function
  | "alpha" -> Re.alpha
  | "alnum" -> Re.alnum
  | "ascii" -> Re.ascii
  | "blank" -> Re.blank
  | "cntrl" -> Re.cntrl
  | "digit" -> Re.digit
  | "lower" -> Re.lower
  | "print" -> Re.print
  | "space" -> Re.space
  | "upper" -> Re.upper
  | "word" -> Re.wordc
  | "punct" -> Re.punct
  | "graph" -> Re.graph
  | "xdigit" -> Re.xdigit
  | class_ -> invalid_arg ("Invalid pcre class: " ^ class_)
;;

let names =
  [ "alpha"
  ; "alnum"
  ; "ascii"
  ; "blank"
  ; "cntrl"
  ; "digit"
  ; "lower"
  ; "print"
  ; "space"
  ; "upper"
  ; "word"
  ; "punct"
  ; "graph"
  ; "xdigit"
  ]
;;

let parse buf =
  let accept = Parse_buffer.accept buf in
  let accept_s = Parse_buffer.accept_s buf in
  match accept ':' with
  | false -> None
  | true ->
    let compl = accept '^' in
    let cls =
      try List.find accept_s names with
      | Not_found -> raise Parse_buffer.Parse_error
    in
    if not (accept_s ":]") then raise Parse_buffer.Parse_error;
    let posix_class = of_name cls in
    Some (if compl then Re.compl [ posix_class ] else posix_class)
;;
