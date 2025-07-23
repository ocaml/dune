{
type 'command block =
  | Command of 'command
  | Comment of string list
}

let eol = '\n' | eof

let blank = [' ' '\t' '\r' '\012']

let indent = "  " | "\t"

rule block = parse
  | eof { None }
  | indent "$ " ([^'\n']* as str) eol
    { Some (command_cont [str] lexbuf) }
  | indent [^'\n']* eol
    { output [] lexbuf }
  | ' '? as str eol
    { comment [str] lexbuf }
  | ' '? [^' ' '\n'] [^'\n']* as str eol
    { comment [str] lexbuf }

and comment acc = parse
  | eof
    { match acc with
    | [] -> None
    | _ -> Some (Comment (List.rev acc))
    }
  | ' '? as str eol
    { comment (str :: acc) lexbuf }
  | ' '? [^' ' '\n'] [^'\n']* as str eol
    { comment (str :: acc) lexbuf }
  | ""
    { Some (Comment (List.rev acc)) }

and output maybe_comment = parse
  | eof
    { match maybe_comment with
    | [] -> None
    | l -> Some (Comment (List.rev l))
    }
  | ' ' eof
    { Some (Comment (List.rev (" " :: maybe_comment))) }
  | indent? eof
    { None }
  | indent eol
    { output [] lexbuf }
  | ' '? as s eol
    { output (s :: maybe_comment) lexbuf }
  | indent "$" eol
    { output [] lexbuf }
  | indent "$" [^' ' '\n'] [^'\n']* eol
    { output [] lexbuf }
  | indent [^'$' '\n'] [^'\n']* eol
    { output [] lexbuf }
  | ""
    { match maybe_comment with
    | [] -> block lexbuf
    | l -> comment l lexbuf
    }

and command_cont acc = parse
  | indent "> " ([^'\n']* as str) eol
    { command_cont (str :: acc) lexbuf }
  | indent ">" eol
    { command_cont ("" :: acc) lexbuf }
  | ""
    { Command (List.rev acc)  }
