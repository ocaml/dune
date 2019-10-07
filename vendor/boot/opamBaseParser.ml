open OpamParserTypes
let main _lex _lexbuf fn =
  { file_contents = []
  ; file_name     = fn
  }
let value _lex _lexbuf = assert false
