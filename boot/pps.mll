{
  let b = Buffer.create 512
}

rule pp = parse
 | "let%expect_test" { skip lexbuf }
 | _ as c { Buffer.add_char b c; pp lexbuf }
 | eof { () }

and skip = parse
  | ";;" { pp lexbuf }
  | _ { skip lexbuf }
  | eof { failwith "unterminated let%expect_test" }

{
  let pp s =
    let lb = Lexing.from_string s in
    Buffer.clear b;
    pp lb;
    Buffer.contents b
}
