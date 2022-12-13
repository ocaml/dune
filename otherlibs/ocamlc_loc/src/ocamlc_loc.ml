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
  | Alert { name; source } ->
    variant "Alert"
      [ record [ ("name", string name); ("source", string source) ] ]

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

  val push : t -> Lexer.token -> unit

  val next : t -> Lexer.token
end = struct
  type t =
    { lexbuf : Lexing.lexbuf
    ; mutable unread : Lexer.token list
    }

  let create lexbuf = { lexbuf; unread = [] }

  let push t token = t.unread <- token :: t.unread

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

let indent_of_severity = function
  | Error _ -> String.length "Error: "
  | Warning _ -> String.length "Warning: "
  | Alert { name; source } ->
    String.length "Alert :" + String.length name + String.length source + 1

let severity tokens =
  match Tokens.peek tokens with
  | Line { contents; indent } -> (
    match Lexer.severity (Lexing.from_string contents) with
    | None -> raise Unknown_format
    | Some (severity, new_contents) ->
      Tokens.junk tokens;
      let indent = indent_of_severity severity + indent in
      Tokens.push tokens (Line { indent; contents = new_contents });
      severity)
  | _ -> raise Unknown_format

let rec skip_excerpt tokens =
  match Tokens.peek tokens with
  | Line { contents; indent = _ } -> (
    match Lexer.skip_excerpt (Lexing.from_string contents) with
    | `Continue ->
      Tokens.junk tokens;
      skip_excerpt tokens
    | `Stop -> ())
  | _ -> ()

let rec acc_message tokens min_indent acc =
  match Tokens.peek tokens with
  | Line line ->
    Tokens.junk tokens;
    let min_indent = min min_indent line.indent in
    acc_message tokens min_indent (line :: acc)
  | _ ->
    List.rev_map acc ~f:(fun { indent; contents } ->
        let prefix = String.make (indent - min_indent) ' ' in
        prefix ^ contents)
    |> String.concat "\n" |> String.trim

let rec related tokens acc =
  match Tokens.peek tokens with
  | Loc { indent; message; loc } ->
    if indent = 0 then List.rev acc
    else (
      Tokens.junk tokens;
      let message =
        acc_message tokens indent [ { indent; contents = message } ]
      in
      let acc = (loc, message) :: acc in
      related tokens acc)
  | _ -> List.rev acc

let toplevel tokens =
  match Tokens.next tokens with
  | Loc { indent; message; loc } ->
    if indent > 0 then raise Unknown_format;
    skip_excerpt tokens;
    let severity = severity tokens in
    let indent = indent + indent_of_severity severity in
    let message =
      acc_message tokens indent [ { indent; contents = message } ]
    in
    let related = related tokens [] in
    { loc; severity; message; related }
  | _ -> raise Unknown_format

let parse s =
  let lexbuf = Lexing.from_string s in
  let tokens = Tokens.create lexbuf in
  let rec loop acc =
    match toplevel tokens with
    | exception Unknown_format -> List.rev acc
    | t -> loop (t :: acc)
  in
  loop []

let dyn_of_raw =
  Dyn.list (function
    | `Loc loc -> dyn_of_loc loc
    | `Message m -> Dyn.string m)

let parse_raw s =
  let lexbuf = Lexing.from_string s in
  let tokens = Tokens.create lexbuf in
  let rec loop acc =
    match Tokens.peek tokens with
    | Loc { loc; message; indent } ->
      Tokens.junk tokens;
      let acc = `Loc loc :: acc in
      let message =
        acc_message tokens indent [ { contents = message; indent } ]
      in
      let acc = `Message message :: acc in
      loop acc
    | Line line ->
      Tokens.junk tokens;
      let message = acc_message tokens line.indent [ line ] in
      let acc = `Message message :: acc in
      loop acc
    | Eof ->
      Tokens.junk tokens;
      List.rev acc
  in
  loop []
