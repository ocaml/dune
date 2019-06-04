open Stdune

exception CParse_error of string

type t = Sexp.t

module type Stream = sig
  type t

  val input_byte : t -> int

  val input_string : t -> int -> string
end

module ChannelStream : Stream with type t = in_channel = struct
  type t = in_channel

  let input_byte = input_byte

  let input_string = really_input_string
end

type string_stream = {data: string; mutable pos: int}

module StringStream : Stream with type t = string_stream = struct
  type t = string_stream

  let input_byte s =
    s.pos <- s.pos + 1 ;
    int_of_char s.data.[s.pos - 1]

  let input_string s len =
    s.pos <- s.pos + len ;
    String.sub s.data ~pos:(s.pos - len) ~len
end

module Parser (S : Stream) = struct
  let parse chan =
    let rec read_size acc c =
      if c == int_of_char ':' then acc
      else
        let idx = c - int_of_char '0' in
        if idx < 0 || idx > 9 then
          raise
            (CParse_error
               (Printf.sprintf "invalid character in size: %c" (char_of_int c)))
        else read_size ((10 * acc) + idx) (S.input_byte chan)
    in
    let rec parse c =
      if c == int_of_char '(' then Sexp.List (parse_list ())
      else Atom (S.input_string chan (read_size 0 c))
    and parse_list () =
      let c = S.input_byte chan in
      if c == int_of_char ')' then []
      else
        let head = parse c in
        head :: parse_list ()
    in
    parse (S.input_byte chan)
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

let parse_canonical str = StringParser.parse {data= str; pos= 0}

let parse_channel_canonical chan = ChannelParser.parse chan
