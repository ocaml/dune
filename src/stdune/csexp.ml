exception Parse_error of string

type t = Sexp.t

let peek s =
  match Stream.peek s with
  | Some v ->
      v
  | None ->
      raise (Parse_error "unexpected end of file")

let read_string s l =
  let res = Bytes.make l ' ' in
  let rec read = function
    | v when v = l ->
        ()
    | v ->
        BytesLabels.set res v (Stream.next s) ;
        read (v + 1)
  in
  try read 0 ; Bytes.to_string res
  with Stream.Failure ->
    raise
      (Parse_error
         (Printf.sprintf "unexpected end of file in atom of size %i" l))

let parse stream =
  let rec read_size acc =
    let c = Stream.next stream in
    if c = ':' then acc
    else
      let idx = int_of_char c - int_of_char '0' in
      if idx < 0 || idx > 9 then
        raise (Parse_error (Printf.sprintf "invalid character in size: %c" c))
      else read_size ((10 * acc) + idx)
  in
  let rec parse () =
    match peek stream with
    | '(' ->
        Stream.junk stream ;
        Sexp.List (parse_list ())
    | _ ->
        Atom (read_string stream (read_size 0))
  and parse_list () =
    match peek stream with
    | ')' ->
        Stream.junk stream ; []
    | ':' ->
        raise (Parse_error "missing size")
    | _ ->
        let head = parse () in
        head :: parse_list ()
  in
  parse ()

let buffer () = Buffer.create 1024

let to_buffer ~buf sexp =
  let rec loop = function
    | Sexp.Atom str ->
        Buffer.add_string buf (string_of_int (String.length str)) ;
        Buffer.add_string buf ":" ;
        Buffer.add_string buf str
    | Sexp.List e ->
        Buffer.add_char buf '(' ;
        ignore (List.map ~f:loop e) ;
        Buffer.add_char buf ')'
  in
  ignore (loop sexp)

let to_string sexp =
  let buf = buffer () in
  to_buffer sexp ~buf ; Buffer.contents buf
