module type Sexp = sig
  type t =
    | Atom of string
    | List of t list
end

module Make (Sexp : Sexp) = struct
  open Sexp

  module type Input = sig
    type t

    val read_string : t -> int -> string

    val read_char : t -> char
  end

  exception Parse_error of string

  let parse_error msg = raise_notrace (Parse_error msg)

  let invalid_character () = parse_error "invalid character"

  let missing_left_parenthesis () =
    parse_error "right parenthesis without matching left parenthesis"

  module Make_parser (Input : Input) = struct
    let int_of_digit c = Char.code c - Char.code '0'

    let rec parse_atom input len =
      match Input.read_char input with
      | '0' .. '9' as c ->
        let len = (len * 10) + int_of_digit c in
        if len > Sys.max_string_length then
          parse_error "atom too big to represent"
        else
          parse_atom input len
      | ':' ->
        let s = Input.read_string input len in
        Atom s
      | _ -> invalid_character ()

    let rec parse_many input depth acc =
      match Input.read_char input with
      | '(' ->
        let sexps = parse_many input (depth + 1) [] in
        parse_many input (depth + 1) (List sexps :: acc)
      | ')' ->
        if depth = 0 then
          missing_left_parenthesis ()
        else
          List.rev acc
      | '0' .. '9' as c ->
        let sexp = parse_atom input (int_of_digit c) in
        parse_many input depth (sexp :: acc)
      | _ -> invalid_character ()

    let parse_one input =
      match Input.read_char input with
      | '(' ->
        let sexps = parse_many input 1 [] in
        List sexps
      | ')' -> missing_left_parenthesis ()
      | '0' .. '9' as c -> parse_atom input (int_of_digit c)
      | _ -> invalid_character ()
  end
  [@@inlined always]

  let premature_end = "premature end of input"

  module String_input = struct
    type t =
      { buf : string
      ; mutable pos : int
      }

    let read_string t len =
      let pos = t.pos in
      let s = String.sub t.buf pos len in
      t.pos <- pos + len;
      s

    let read_char t =
      let pos = t.pos in
      let c = t.buf.[pos] in
      t.pos <- pos + 1;
      c

    let error_of_exn t = function
      | Parse_error msg -> Error (t.pos, msg)
      | _ -> Error (t.pos, premature_end)
  end

  module String_parser = Make_parser (String_input)

  let parse_string s =
    let input : String_input.t = { buf = s; pos = 0 } in
    match String_parser.parse_one input with
    | x ->
      if input.pos <> String.length s then
        Error (input.pos, "data after canonical S-expression")
      else
        Ok x
    | exception e -> String_input.error_of_exn input e

  let parse_string_many s =
    let input : String_input.t = { buf = s; pos = 0 } in
    match String_parser.parse_many input 0 [] with
    | l -> Ok (List.rev l)
    | exception e -> String_input.error_of_exn input e

  module In_channel_input = struct
    type t = in_channel

    let read_string = really_input_string

    let read_char = input_char
  end

  module In_channel_parser = Make_parser (In_channel_input)

  let input_opt ic =
    let pos = LargeFile.pos_in ic in
    match In_channel_parser.parse_one ic with
    | x -> Ok (Some x)
    | exception End_of_file ->
      if LargeFile.pos_in ic = pos then
        Ok None
      else
        Error premature_end
    | exception Parse_error msg -> Error msg

  let input ic =
    match input_opt ic with
    | Ok None -> Error premature_end
    | Ok (Some x) -> Ok x
    | Error msg -> Error msg

  let input_many =
    let rec loop ic acc =
      match input_opt ic with
      | Error _ as res -> res
      | Ok None -> Ok (List.rev acc)
      | Ok (Some x) -> loop ic (x :: acc)
    in
    fun ic -> loop ic []

  let serialised_length =
    let rec loop acc t =
      match t with
      | Atom s ->
        let len = String.length s in
        let x = ref len in
        let len_len = ref 1 in
        while !x > 9 do
          x := !x / 10;
          incr len_len
        done;
        acc + !len_len + 1 + len
      | List l -> List.fold_left loop acc l
    in
    fun t -> loop 0 t

  let to_buffer buf sexp =
    let rec loop = function
      | Atom str ->
        Buffer.add_string buf (string_of_int (String.length str));
        Buffer.add_string buf ":";
        Buffer.add_string buf str
      | List e ->
        Buffer.add_char buf '(';
        List.iter loop e;
        Buffer.add_char buf ')'
    in
    loop sexp

  let to_string sexp =
    let buf = Buffer.create (serialised_length sexp) in
    to_buffer buf sexp;
    Buffer.contents buf

  let to_channel oc sexp =
    let rec loop = function
      | Atom str ->
        output_string oc (string_of_int (String.length str));
        output_char oc ':';
        output_string oc str
      | List l ->
        output_char oc '(';
        List.iter loop l;
        output_char oc ')'
    in
    loop sexp
end
