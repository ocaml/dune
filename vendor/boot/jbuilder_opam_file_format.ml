module OpamParserTypes = struct
  type value =
    | String of unit * string
    | List of unit * value list
    | Other

  type opamfile_item =
    | Variable of unit * string * value
    | Other

  type opamfile =
    { file_contents : opamfile_item list
    ; file_name     : string
    }
end
module OpamBaseParser = struct
  open OpamParserTypes
  let main _lex _lexbuf fn =
    assert (fn = "jbuilder.opam");
    { file_contents = []
    ; file_name     = fn
    }
end
module OpamLexer = struct
  exception Error of string
  let token _ = assert false
end
