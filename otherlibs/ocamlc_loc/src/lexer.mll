{
  (* raise when the format is unrecognized *)
  exception Unknown_format

  type lines =
    | Single of int
    | Range of int * int

  type source =
    | Code of { code : int ; name : string }
    | Alert of string

  type severity =
    | Error of source option
    | Warning of source
    | Alert of { name : string ; source : string }

  type loc =
    { chars : (int * int) option
    ; lines : lines
    ; path : string
    }

  type line = { indent : int ; contents : string }

  type token =
    | Loc of { indent : int ; loc : loc ; message : string }
    | Line of line
    | Eof

  let parse_range s =
    match String.split_on_char '-' s with
    | [x; y] -> (int_of_string x, int_of_string y)
    | _ -> assert false
}

let blank = [' ' '\t' '\r']*

let newline = '\n'

let quoted_string = '"' [^ '"']+ '"'

let digits = ['0' - '9']+

let range = digits "-" digits

let any = _ *

let alert_name = ['a' - 'z'] ['A' - 'Z' 'a' - 'z' '0' - '9' '_']*

rule skip_excerpt = parse
  | blank digits " | " [^ '\n']* "\n"?
    { `Continue }
  | blank '^'+ blank "\n"?
    { `Continue }
  | eof { `Stop }
  | "" { `Stop }

and severity = parse
  | "Error:"
    (blank any as rest)
    { Some (Error None, rest) }
  | "Warning" blank (digits as code) blank "[" ([^ ']']+ as name) "]:"
    (blank any as rest)
    { Some (Warning (Code { code = int_of_string code ; name }), rest)
    }
  | "Error" blank
    "(warning" blank (digits as code) blank "[" ([^ ']']+ as name) "]):"
    (blank any as rest)
    { Some (Error (Some (Code { code = int_of_string code ; name })), rest)
    }
    | "Alert " blank (alert_name as name) ":" blank (any as source)
    {  Some (Alert { name ; source }, "")
    }
  | (("Error" | "Warning") as kind) " (alert " ([^ ')']+ as alert) "):"
    (blank any as rest)
    { let alert : source = Alert alert in
      let res =
        match kind with
        | "Error" -> Error (Some alert)
        | "Warning" -> Warning alert
        | _ -> assert false
      in
      Some (res, rest)
    }
  | "" { None }

and line = parse
  | (blank as prefix) ([^ '\n']* as contents) blank newline?
    { Line { indent = String.length prefix ; contents }
    }
  | eof { Eof }

and token = parse
  | (blank as indent) "File \"" ([^ '"']* as path) "\", " blank
    (("line " (digits as line) | "lines " (range as lines)))
    ("," blank "characters" blank (range as chars))?
    ":" blank ([^ '\n']* as message) newline?
    { let lines =
        match line, lines with
        | Some line, None -> Single (int_of_string line)
        | None, Some range ->
          let start, finish = (parse_range range) in
          Range (start, finish)
        | None, None
        | Some _, Some _ -> assert false
      in
      let chars =
        match chars with
        | None -> None
        | Some chars ->
          let start, finish = parse_range chars in
          Some (start, finish)
      in
      let indent = String.length indent in
      let loc = { lines ; path ; chars } in
      Loc { loc ; indent ; message }
    }
  | eof { Eof }
  | "" { line lexbuf }
