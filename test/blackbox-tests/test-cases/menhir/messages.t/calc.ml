module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module I = Parser.MenhirInterpreter

let env checkpoint =
  match checkpoint with
  | I.HandlingError env ->
      env
  | _ ->
      assert false

let state checkpoint : int =
  match I.top (env checkpoint) with
  | Some (I.Element (s, _, _, _)) ->
      I.number s
  | None ->
      0

let show text positions =
  E.extract text positions
  |> E.sanitize
  |> E.compress
  |> E.shorten 20

let get text checkpoint i =
  match I.get i (env checkpoint) with
  | Some (I.Element (_, _, pos1, pos2)) ->
      show text (pos1, pos2)
  | None ->
      "???"

let succeed v =
  Printf.printf "%d\n%!" v;
  exit 0

let fail text buffer (checkpoint : _ I.checkpoint) =
  let location = L.range (E.last buffer) in
  let indication = Printf.sprintf "Syntax error %s.\n" (E.show (show text) buffer) in
  let message = ParserMessages.message (state checkpoint) in
  let message = E.expand (get text checkpoint) message in
  Printf.eprintf "%s%s%s%!" location indication message;
  exit 1

let main text =
  let lexbuf = L.init "<input>" (Lexing.from_string text) in
  let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  let buffer, supplier = E.wrap_supplier supplier in
  let checkpoint = Parser.Incremental.main lexbuf.lex_curr_p in
  I.loop_handle succeed (fail text buffer) supplier checkpoint

let () =
  main Sys.argv.(1)
