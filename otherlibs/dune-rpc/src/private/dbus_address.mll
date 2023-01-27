
{
(*
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)
  exception Fail of int * string

  let pos lexbuf = lexbuf.Lexing.lex_start_p.Lexing.pos_cnum

  let fail lexbuf fmt =
    Printf.ksprintf
      (fun msg -> raise (Fail(pos lexbuf, msg)))
      fmt

  let decode_char ch = match ch with
    | '0'..'9' -> Char.code ch - Char.code '0'
    | 'a'..'f' -> Char.code ch - Char.code 'a' + 10
    | 'A'..'F' -> Char.code ch - Char.code 'A' + 10
    | _ -> raise (Invalid_argument "decode_char")

  let hex_decode hex =
    if String.length hex mod 2 <> 0 then raise (Invalid_argument "OBus_util.hex_decode");
    let len = String.length hex / 2 in
    let str = Bytes.create len in
    for i = 0 to len - 1 do
      Bytes.unsafe_set str i
        (char_of_int
           ((decode_char (String.unsafe_get hex (i * 2)) lsl 4) lor
              (decode_char (String.unsafe_get hex (i * 2 + 1)))))
    done;
    Bytes.unsafe_to_string str

  type t =
    { name : string
    ; args : (string * string) list
    }

  type error =
    { position : int
    ; reason : string
    }

}

let name = [^ ':' ',' ';' '=']+

rule address = parse
    | name as name {
        check_colon lexbuf;
        let args = parameters lexbuf in
        check_eof lexbuf;
        { name ; args }
      }
    | ":" {
        fail lexbuf "empty transport name"
      }
    | eof {
        fail lexbuf "address expected"
      }

and check_eof = parse
    | eof { () }
    | _ as ch { fail lexbuf "invalid character %C" ch }

and check_colon = parse
    | ":" { () }
    | "" { fail lexbuf "colon expected after transport name" }

and parameters = parse
    | name as key {
        check_equal lexbuf;
        let value = value (Buffer.create 42) lexbuf in
        if coma lexbuf then
          (key, value) :: parameters_plus lexbuf
        else
          [(key, value)]
      }
    | "=" { fail lexbuf "empty key" }
    | "" { [] }

and parameters_plus = parse
    | name as key {
        check_equal lexbuf;
        let value = value (Buffer.create 42) lexbuf in
        if coma lexbuf then
          (key, value) :: parameters_plus lexbuf
        else
          [(key, value)]
      }
    | "=" { fail lexbuf "empty key" }
    | "" { fail lexbuf "parameter expected" }

and coma = parse
    | "," { true }
    | "" { false }

and check_equal = parse
    | "=" { () }
    | "" { fail lexbuf "equal expected after key" }

and value buf = parse
    | [ '0'-'9' 'A'-'Z' 'a'-'z' '_' '-' '/' '.' '\\' ] as ch {
        Buffer.add_char buf ch;
        value buf lexbuf
      }
    | "%" {
        Buffer.add_string buf (unescape lexbuf);
        value buf lexbuf
      }
    | "" {
        Buffer.contents buf
      }

and unescape = parse
    | [ '0'-'9' 'a'-'f' 'A'-'F' ] [ '0'-'9' 'a'-'f' 'A'-'F' ] as str
        { hex_decode str }
    | ""
        { fail lexbuf "two hexdigits expected after '%%'" }

{
  let of_string str =
    try
      Ok (address (Lexing.from_string str))
    with Fail(position, reason) ->
      Error { position ; reason }

  let to_string { name ; args } =
    let buf = Buffer.create 42 in
    let escape = String.iter begin fun ch -> match ch with
      | '0'..'9' | 'A'..'Z' | 'a'..'z'
      | '_' | '-' | '/' | '.' | '\\' ->
          Buffer.add_char buf ch
      | _ ->
          Printf.bprintf buf "%%%02x" (Char.code ch)
    end in
    let concat ch f = function
      | [] -> ()
      | x :: l -> f x; List.iter (fun x -> Buffer.add_char buf ch; f x) l
    in
    Buffer.add_string buf name;
    Buffer.add_char buf ':';
    concat ','
      (fun (k, v) ->
         Buffer.add_string buf k;
         Buffer.add_char buf '=';
         escape v)
      args;
    Buffer.contents buf

}
