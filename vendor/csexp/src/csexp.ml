module type Sexp = sig
  type t =
    | Atom of string
    | List of t list
end

module type Monad = sig
  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Make (Sexp : Sexp) = struct
  open Sexp
  include Result

  module type Input = sig
    type t

    module Monad : Monad

    val read_string : t -> int -> (string, string) result Monad.t

    val read_char : t -> (char, string) result Monad.t
  end

  let parse_error f = Format.ksprintf (fun msg -> Result.Error msg) f

  let invalid_character c = parse_error "invalid character %C" c

  let missing_left_parenthesis () =
    parse_error "right parenthesis without matching left parenthesis"

  module Make_parser (Input : Input) = struct
    let int_of_digit c = Char.code c - Char.code '0'

    let ( >>= ) = Input.Monad.bind

    open Input.Monad

    let rec parse_atom input len =
      Input.read_char input >>= function
      | Ok ('0' .. '9' as c) ->
        let len = (len * 10) + int_of_digit c in
        if len > Sys.max_string_length then
          return @@ parse_error "atom too big to represent"
        else
          parse_atom input len
      | Ok ':' -> (
        Input.read_string input len >>= function
        | Ok s -> return @@ Ok (Atom s)
        | Error e -> return @@ Error e )
      | Ok c -> return @@ invalid_character c
      | Error e -> return @@ Error e

    let rec parse_many depth input acc =
      Input.read_char input >>= function
      | Ok '(' -> (
        parse_many (depth + 1) input [] >>= function
        | Ok sexps -> parse_many (depth + 1) input @@ (List sexps :: acc)
        | e -> return e )
      | Ok ')' ->
        if depth = 0 then
          return @@ missing_left_parenthesis ()
        else
          return @@ Ok (List.rev acc)
      | Ok c when '0' <= c && c <= '9' -> (
        parse_atom input (int_of_digit c) >>= function
        | Ok sexp -> parse_many depth input (sexp :: acc)
        | Error e -> return @@ Error e )
      | Ok c -> return @@ invalid_character c
      | Error e -> return @@ Error e

    let parse input =
      Input.read_char input >>= function
      | Ok '(' -> (
        parse_many 1 input [] >>= function
        | Ok sexps -> return @@ Ok (List sexps)
        | Error e -> return @@ Error e )
      | Ok ')' -> return @@ missing_left_parenthesis ()
      | Ok c when '0' <= c && c <= '9' -> parse_atom input (int_of_digit c)
      | Ok c -> return @@ invalid_character c
      | Error e -> return @@ Error e

    let parse_many input = parse_many 0 input []
  end
  [@@inlined always]

  let premature_end = "premature end of input"

  module Id_monad = struct
    type 'a t = 'a

    let return x = x

    let bind x f = f x
  end

  module String_input = struct
    type t =
      { buf : string
      ; mutable pos : int
      }

    module Monad = Id_monad

    let read_string t len =
      let pos = t.pos in
      if pos + len <= String.length t.buf then (
        let s = String.sub t.buf pos len in
        t.pos <- pos + len;
        Ok s
      ) else
        Error premature_end

    let read_char t =
      if t.pos + 1 <= String.length t.buf then (
        let pos = t.pos in
        let c = t.buf.[pos] in
        t.pos <- pos + 1;
        Ok c
      ) else
        Error premature_end
  end

  module String_parser = Make_parser (String_input)

  let parse_string s =
    let input : String_input.t = { buf = s; pos = 0 } in
    match String_parser.parse input with
    | Ok parsed ->
      if input.pos <> String.length s then
        Error (input.pos, "data after canonical S-expression")
      else
        Ok parsed
    | Error msg -> Error (input.pos, msg)

  let parse_string_many s =
    let input : String_input.t = { buf = s; pos = 0 } in
    match String_parser.parse_many input with
    | Ok l -> Ok (List.rev l)
    | Error e -> Error (input.pos, e)

  module In_channel_input = struct
    type t = in_channel

    module Monad = Id_monad

    let read_string size input =
      try Ok (really_input_string size input)
      with End_of_file -> Error premature_end

    let read_char input =
      try Ok (input_char input) with End_of_file -> Error premature_end
  end

  module In_channel_parser = Make_parser (In_channel_input)

  let input_opt ic =
    let pos = LargeFile.pos_in ic in
    match In_channel_parser.parse ic with
    | Ok x -> Ok (Some x)
    | Error msg -> Error msg
    | exception End_of_file ->
      if LargeFile.pos_in ic = pos then
        Ok None
      else
        Error premature_end

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
