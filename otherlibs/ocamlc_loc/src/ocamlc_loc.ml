include Lexer
module List = ListLabels

type report =
  { loc : loc
  ; severity : severity
  ; message : string
  ; related : (loc * string) list
  }

let dyn_of_source =
  let open Dyn in
  function
  | Code { code; name } -> record [ ("code", int code); ("name", string name) ]
  | Alert s -> string s

let dyn_of_severity =
  let open Dyn in
  function
  | Error w -> variant "Error" [ option dyn_of_source w ]
  | Warning w -> variant "Warning" [ dyn_of_source w ]

let dyn_of_loc { path; lines; chars } =
  let open Dyn in
  record
    [ ("path", string path)
    ; ( "line"
      , match lines with
        | Single i -> variant "Single" [ int i ]
        | Range (i, j) -> variant "Range" [ int i; int j ] )
    ; ("chars", option (pair int int) chars)
    ]

let dyn_of_report { loc; message; related; severity } =
  let open Dyn in
  record
    [ ("loc", dyn_of_loc loc)
    ; ("message", string message)
    ; ("related", list (pair dyn_of_loc string) related)
    ; ("severity", dyn_of_severity severity)
    ]

module Tokens : sig
  type t

  val create : Lexing.lexbuf -> t

  val peek : t -> Lexer.token

  val junk : t -> unit

  val next : t -> Lexer.token
end = struct
  type t =
    { lexbuf : Lexing.lexbuf
    ; mutable unread : Lexer.token list
    }

  let create lexbuf = { lexbuf; unread = [] }

  let next t =
    match t.unread with
    | [] -> Lexer.token t.lexbuf
    | x :: xs ->
      t.unread <- xs;
      x

  let peek t =
    match t.unread with
    | x :: _ -> x
    | [] ->
      let token = Lexer.token t.lexbuf in
      t.unread <- [ token ];
      token

  let junk t =
    match t.unread with
    | _ :: xs -> t.unread <- xs
    | _ -> ignore (Lexer.token t.lexbuf)
end

let parse lexbuf =
  let tokens = Tokens.create lexbuf in
  let rec acc_message min_indent acc =
    match Tokens.peek tokens with
    | Line line ->
      Tokens.junk tokens;
      let min_indent = min min_indent line.indent in
      acc_message min_indent (line :: acc)
    | _ ->
      List.rev_map acc ~f:(fun { indent; contents } ->
          let prefix = String.make (indent - min_indent) ' ' in
          prefix ^ contents)
      |> String.concat "\n" |> String.trim
  in
  let rec related acc =
    match Tokens.peek tokens with
    | Related { indent; loc; message } ->
      Tokens.junk tokens;
      let message = acc_message indent [ { indent; contents = message } ] in
      related ((loc, message) :: acc)
    | _ -> List.rev acc
  in
  let rec toplevel acc =
    match Tokens.next tokens with
    | Toplevel { indent; loc; severity; message } ->
      let message =
        let indent =
          indent
          +
          match severity with
          | Error _ -> String.length "Error: "
          | Warning _ -> String.length "Warning: "
        in
        acc_message indent [ { indent; contents = message } ]
      in
      let related = related [] in
      let acc = { severity; loc; message; related } :: acc in
      toplevel acc
    | Eof -> acc
    | _ -> raise Unknown_format
  in
  try List.rev @@ toplevel [] with Unknown_format -> []

let parse s = parse (Lexing.from_string s)
