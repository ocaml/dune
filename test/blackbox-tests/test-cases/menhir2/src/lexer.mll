{
open Tokens
}

rule lex = parse
  | 'c' { TOKEN 'c' }
  | eof { EOF }
