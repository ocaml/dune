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

  type loc =
    { chars : (int * int) option
    ; lines : lines
    ; path : string
    }

  type line = { indent : int ; contents : string }

  type token =
    | Toplevel of
      { indent : int
      ; loc : loc
      ; severity : severity
      ; message : string
      }
    | Related of { indent : int ; loc : loc ; message: string }
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

rule skip_excerpt = parse
  | blank digits " | " [^ '\n']* "\n"
    { skip_excerpt lexbuf }
  | blank '^'+ blank "\n"
    { () }
  | eof { () }
  | "" { () }

and severity = parse
  | "Error:" blank { Error None }
  | "Warning" blank (digits as code) blank "[" ([^ ']']+ as name) "]:" blank
    { Warning (Code { code = int_of_string code ; name })
    }
  | "Error" blank
    "(warning" blank (digits as code) blank "[" ([^ ']']+ as name) "]):"
    blank
    { Error (Some (Code { code = int_of_string code ; name }))
    }
  | (("Error" | "Warning") as kind) " (alert " ([^ ')']+ as alert) "):"
    { let alert = Alert alert in
      match kind with
      | "Error" -> Error (Some alert)
      | "Warning" -> Warning alert
      | _ -> assert false
    }
  | "" { raise_notrace Unknown_format }

and line = parse
  | (blank as prefix) ([^ '\n']* as contents) blank newline?
    { Line { indent = String.length prefix ; contents }
    }
  | eof { Eof }

and toplevel_message = parse
  | blank ([^ '\n']* as message) blank '\n' { message }
  | "" { "" }

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
      let severity, message =
        if indent > 0 then begin
          (None, message)
        end else begin 
          skip_excerpt lexbuf;
          let severity = severity lexbuf in
          let message = toplevel_message lexbuf in
          (Some severity, message)
        end
      in
      match severity with
      | None -> Related { loc ; indent ; message }
      | Some severity -> Toplevel { loc ; severity; indent ; message }
    }
  | eof { Eof }
  | "" { line lexbuf }
