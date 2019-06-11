exception Parse_error of string

type t = Sexp.t

module type Stream = sig
  type input

  type t

  val make : input -> t

  val peek_byte : t -> int

  val input_byte : t -> int

  val input_string : t -> int -> string
end

module ChannelStream :
  Stream with type input = in_channel and type t = in_channel * int option ref =
struct
  type input = in_channel

  type t = in_channel * int option ref

  let make i = (i, ref None)

  let peek_byte (chan, peek) =
    match !peek with
    | None ->
        let b = input_byte chan in
        peek := Some b ;
        b
    | Some b ->
        b

  let input_byte (chan, peek) =
    match !peek with
    | None ->
        input_byte chan
    | Some b ->
        peek := None ;
        b

  let input_string (chan, peek) len =
    match !peek with
    | None ->
        really_input_string chan len
    | Some b ->
        peek := None ;
        String.make 1 (char_of_int b) ^ really_input_string chan (len - 1)
end

type string_stream = {data: string; mutable pos: int}

module StringStream :
  Stream with type input = string and type t = string_stream = struct
  type input = string

  type t = string_stream

  let make str = {data= str; pos= 0}

  let peek_byte s = int_of_char s.data.[s.pos]

  let input_byte s =
    let b = peek_byte s in
    s.pos <- s.pos + 1 ;
    b

  let input_string s len =
    s.pos <- s.pos + len ;
    String.sub s.data ~pos:(s.pos - len) ~len
end

module Parser (S : Stream) = struct
  let parse_stream chan =
    let rec read_size acc =
      let c = S.input_byte chan in
      if c = int_of_char ':' then acc
      else
        let idx = c - int_of_char '0' in
        if idx < 0 || idx > 9 then
          raise
            (Parse_error
               (Printf.sprintf "invalid character in size: %c" (char_of_int c)))
        else read_size ((10 * acc) + idx)
    in
    let rec parse () =
      let c = S.peek_byte chan in
      if c = int_of_char '(' then (
        ignore (S.input_byte chan) ;
        Sexp.List (parse_list ()) )
      else Atom (S.input_string chan (read_size 0))
    and parse_list () =
      let c = S.peek_byte chan in
      if c = int_of_char ')' then (
        ignore (S.input_byte chan) ;
        [] )
      else
        let head = parse () in
        head :: parse_list ()
    in
    parse ()

  let parse input = parse_stream (S.make input)
end

module ChannelParser = Parser (ChannelStream)
module StringParser = Parser (StringStream)

let buffer () = Buffer.create 1024

let to_buffer_canonical ~buf sexp =
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

let to_string_canonical sexp =
  let buf = buffer () in
  to_buffer_canonical sexp ~buf ;
  Buffer.contents buf

let parse_canonical = StringParser.parse

let parse_channel_canonical = ChannelParser.parse
