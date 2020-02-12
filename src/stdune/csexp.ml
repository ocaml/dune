open Result.O

let ok = Result.ok

type t = Sexp.t

let peek s =
  match Stream.peek s with
  | Some v -> ok v
  | None -> Error "unexpected end of file"

let read_string s l =
  let res = Bytes.make l ' ' in
  let rec read = function
    | v when v = l -> ()
    | v ->
      BytesLabels.set res v (Stream.next s);
      read (v + 1)
  in
  try
    read 0;
    ok (Bytes.to_string res)
  with Stream.Failure ->
    Error (Printf.sprintf "unexpected end of file in atom of size %i" l)

let parse stream =
  let rec read_size acc =
    let c = Stream.next stream in
    if c = ':' then
      ok acc
    else
      let idx = int_of_char c - int_of_char '0' in
      if idx < 0 || idx > 9 then
        Error (Printf.sprintf "invalid character in size: %c" c)
      else
        read_size ((10 * acc) + idx)
  in
  let rec parse () =
    peek stream >>= function
    | '(' ->
      Stream.junk stream;
      parse_list () >>| fun l -> Sexp.List l
    | _ -> read_size 0 >>= read_string stream >>| fun x -> Sexp.Atom x
  and parse_list () =
    peek stream >>= function
    | ')' ->
      Stream.junk stream;
      ok []
    | ':' -> Error "missing size"
    | _ ->
      let head = parse () in
      head >>= fun head ->
      parse_list () >>| fun tail -> head :: tail
  in
  parse ()

let buffer () = Buffer.create 1024

let to_buffer ~buf sexp =
  let rec loop = function
    | Sexp.Atom str ->
      Buffer.add_string buf (string_of_int (String.length str));
      Buffer.add_string buf ":";
      Buffer.add_string buf str
    | Sexp.List e ->
      Buffer.add_char buf '(';
      List.iter e ~f:loop;
      Buffer.add_char buf ')'
  in
  loop sexp

let to_string sexp =
  let buf = buffer () in
  to_buffer sexp ~buf;
  Buffer.contents buf

let to_channel oc sexp =
  let rec loop = function
    | Sexp.Atom str ->
      output_string oc (string_of_int (String.length str));
      output_char oc ':';
      output_string oc str
    | Sexp.List e ->
      output_char oc '(';
      List.iter e ~f:loop;
      output_char oc ')'
  in
  loop sexp

let parse_string string =
  let open Result.O in
  let stream = Stream.of_string string in
  let* result = parse stream in
  match Stream.peek stream with
  | Some _ -> Error "not whole string consumed"
  | None -> Ok result
