{
  open Stdune
}

let digit     = ['0'-'9']

rule deps = parse
  | "deps[" (digit+ as i) "]" eof { Some (Int.of_string_exn i) }
  | _  { None }

{
let deps s = deps (Lexing.from_string s)
}
