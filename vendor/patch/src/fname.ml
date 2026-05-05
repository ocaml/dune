type lexer_output =
  | Quoted of (string * string)
  | Unquoted
  | Error of string

exception Cant_parse_octal

let ascii_zero = 48 (* Char.code '0' *)
let octal_to_char c1 c2 c3 =
  let char_to_digit c = Char.code c - ascii_zero in
  try
    Char.chr (
      (char_to_digit c1 lsl 6) lor
      (char_to_digit c2 lsl 3) lor
      char_to_digit c3
    )
  with Invalid_argument _ -> raise Cant_parse_octal

let lex_quoted_char s len i =
  match s.[i] with
  | 'a' -> Some ('\007', 2)
  | 'b' -> Some ('\b', 2)
  | 'f' -> Some ('\012', 2)
  | 'n' -> Some ('\n', 2)
  | 'r' -> Some ('\r', 2)
  | 't' -> Some ('\t', 2)
  | 'v' -> Some ('\011', 2)
  | '\\' -> Some ('\\', 2)
  | '"' -> Some ('"', 2)
  | '0'..'3' as c1 when len >= 3 ->
      begin match s.[i + 1], s.[i + 2] with
      | ('0'..'7' as c2), ('0'..'7' as c3) ->
          (try Some (octal_to_char c1 c2 c3, 4)
           with Cant_parse_octal -> None)
      | _, _ -> None
      end
  | _ -> None

let rec lex_quoted_filename buf s len i =
  if len > 0 then
    match s.[i] with
    | '"' -> Quoted (Buffer.contents buf, Lib.String.slice ~start:(i + 1) s)
    | '\\' when len > 2 ->
        let char_size =
          match lex_quoted_char s (len - 1) (i + 1) with
          | Some (c, char_size) -> Buffer.add_char buf c; char_size
          | None -> Buffer.add_char buf s.[i]; 1
        in
        lex_quoted_filename buf s (len - char_size) (i + char_size)
    | c ->
        Buffer.add_char buf c;
        lex_quoted_filename buf s (len - 1) (i + 1)
  else
    Unquoted

let lex_filename buf s len =
  if len > 0 then
    match s.[0] with
    | '"' -> lex_quoted_filename buf s (len - 1) 1
    | _ -> Unquoted
  else
    Error "empty filename"

let parse_filename ~allow_space s =
  match lex_filename (Buffer.create 128) s (String.length s) with
  | Quoted x -> Ok x
  | Unquoted when not allow_space ->
      begin match Lib.String.cut ' ' s with
      | None -> Ok (s, "")
      | Some x -> Ok x
      end
  | Unquoted -> Ok (s, "")
  | Error msg -> Error msg

let parse s =
  let filename_and_date =
    match Lib.String.cut '\t' s with
    | None ->
        parse_filename ~allow_space:false s
    | Some (filename, date) ->
        match parse_filename ~allow_space:true filename with
        | Ok (filename, "") -> Ok (filename, date)
        | Ok _ -> Error "Unexpected character after closing double-quote"
        | Error _ as err -> err
  in
  match filename_and_date with
  | Ok (filename, date) ->
      if filename = "/dev/null" ||
         let date = String.trim date in
         Lib.String.is_prefix ~prefix:"1970-" date ||
         Lib.String.is_prefix ~prefix:"1969-" date ||
         Lib.String.is_suffix ~suffix:" 1970" date ||
         Lib.String.is_suffix ~suffix:" 1969" date then
        (* See https://github.com/hannesm/patch/issues/8 *)
        Ok None
      else
        Ok (Some filename)
  | Error _ as err -> err

let parse_git_filename s =
  match parse_filename ~allow_space:true s with
  | Ok (s, "") -> Ok s
  | Ok _ -> Error "Unexpected character after closing double-quote in header"
  | Error _ as err -> err

let parse_git_header_rename ~from_ ~to_ s =
  let rec loop ~s ~len i =
    if i < (len : int) then
      match String.unsafe_get s i with
      | ' ' | '\t' ->
          let a = parse_git_filename (Lib.String.slice ~stop:i s) in
          let b = parse_git_filename (Lib.String.slice ~start:(i + 1) s) in
          begin match a, b with
          | Ok a, Ok b
            when Lib.String.is_suffix ~suffix:from_ a &&
                 Lib.String.is_suffix ~suffix:to_ b
            -> Some (a, b)
          | Ok _, Ok _ | Error _, _ | _, Error _
            -> loop ~s ~len (i + 1)
          end
      | _ -> loop ~s ~len (i + 1)
    else
      None
  in
  loop ~s ~len:(String.length s) 0

let parse_git_header_same s =
  let rec loop ~best ~s ~len i =
    if i < (len : int) then
      match String.unsafe_get s i with
      | ' ' | '\t' ->
          let a = parse_git_filename (Lib.String.slice ~stop:i s) in
          let b = parse_git_filename (Lib.String.slice ~start:(i + 1) s) in
          begin match a, b with
          | Ok a, Ok b ->
              begin match best, Lib.String.count_common_suffix a b with
              | None, best -> loop ~best:(Some (best, a, b)) ~s ~len (i + 1)
              | Some (prev_best, _, _), best when best > (prev_best : int) ->
                  loop ~best:(Some (best, a, b)) ~s ~len (i + 1)
              | Some _ as best, _ -> loop ~best ~s ~len (i + 1)
              end
          | Error _, _ | _, Error _ -> loop ~best ~s ~len (i + 1)
          end
      | _ -> loop ~best ~s ~len (i + 1)
    else
      match best with
      | None -> None
      | Some (_best, a, b) -> Some (a, b)
  in
  loop ~best:None ~s ~len:(String.length s) 0
