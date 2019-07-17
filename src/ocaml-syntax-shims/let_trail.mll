{
  let name = function
    | '!' -> "bang"
    | '$' -> "dollar"
    | '%' -> "percent"
    | '&' -> "ampersand"
    | '*' -> "star"
    | '+' -> "plus"
    | '-' -> "minus"
    | '/' -> "slash"
    | ':' -> "colon"
    | '<' -> "lesser"
    | '=' -> "equal"
    | '>' -> "greater"
    | '?' -> "question"
    | '@' -> "at"
    | '^' -> "circumflex"
    | '|' -> "pipe"
    | _ -> assert false

  let expand s =
    let buf = Buffer.create 128 in
    for i = 0 to String.length s - 1 do
      if i > 0 then Buffer.add_char buf '_';
      Buffer.add_string buf (name s.[i])
    done;
    Buffer.contents buf
}

let dotsymbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '/' ':' '=' '>' '?' '@' '^' '|']
let kwdopchar =
  ['$' '&' '*' '+' '-' '/' '<' '=' '>' '@' '^' '|']

rule op = parse
  | kwdopchar dotsymbolchar* as s { Some (expand s) }
  | "" { None }
