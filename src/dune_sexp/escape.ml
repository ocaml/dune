open Import

(** Note: on OCaml >= 4.14, this can be switched to the following (and the
    dependency to [Uutf] can be removed)

    {[
      let next_valid_utf8_length s i =
        let decode = String.get_utf_8_uchar s i in
        Option.some_if (Uchar.utf_decode_is_valid decode) (Uchar.utf_decode_length decode)
      ;;
    ]} *)
let next_valid_utf8_uchar_len s i =
  let pos = ref i in
  let buf = Bytes.create 1 in
  let decoder = Uutf.decoder ~encoding:`UTF_8 `Manual in
  let rec go () =
    match Uutf.decode decoder with
    | `Await ->
      if !pos >= String.length s
      then None
      else (
        Bytes.set buf 0 (String.get s !pos);
        incr pos;
        Uutf.Manual.src decoder buf 0 1;
        go ())
    | `Uchar _ -> Some (!pos - i)
    | `Malformed _ -> None
    | `End -> Code_error.raise "next_valid_utf8_uchar: `End" []
  in
  go ()
;;

let quote_length s =
  let n = ref 0 in
  let len = String.length s in
  let i = ref 0 in
  while !i < len do
    (n
     := !n
        +
        match String.unsafe_get s !i with
        | '\"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
        | '%' -> if !i + 1 < len && s.[!i + 1] = '{' then 2 else 1
        | ' ' .. '~' -> 1
        | _ ->
          (match next_valid_utf8_uchar_len s !i with
           | Some uchar_len ->
             i := !i + uchar_len - 1;
             uchar_len
           | None -> 4));
    incr i
  done;
  !n
;;

let escape_to s ~dst:s' ~ofs =
  let n = ref ofs in
  let len = String.length s in
  let i = ref 0 in
  while !i < len do
    (match String.unsafe_get s !i with
     | ('\"' | '\\') as c ->
       Bytes.unsafe_set s' !n '\\';
       incr n;
       Bytes.unsafe_set s' !n c
     | '\n' ->
       Bytes.unsafe_set s' !n '\\';
       incr n;
       Bytes.unsafe_set s' !n 'n'
     | '\t' ->
       Bytes.unsafe_set s' !n '\\';
       incr n;
       Bytes.unsafe_set s' !n 't'
     | '\r' ->
       Bytes.unsafe_set s' !n '\\';
       incr n;
       Bytes.unsafe_set s' !n 'r'
     | '\b' ->
       Bytes.unsafe_set s' !n '\\';
       incr n;
       Bytes.unsafe_set s' !n 'b'
     | '%' when !i + 1 < len && s.[!i + 1] = '{' ->
       Bytes.unsafe_set s' !n '\\';
       incr n;
       Bytes.unsafe_set s' !n '%'
     | ' ' .. '~' as c -> Bytes.unsafe_set s' !n c
     | c ->
       (match next_valid_utf8_uchar_len s !i with
        | Some uchar_len ->
          Bytes.unsafe_set s' !n (String.unsafe_get s !i);
          Bytes.unsafe_set s' (!n + 1) (String.unsafe_get s (!i + 1));
          if uchar_len > 2
          then Bytes.unsafe_set s' (!n + 2) (String.unsafe_get s (!i + 2));
          if uchar_len > 3
          then Bytes.unsafe_set s' (!n + 3) (String.unsafe_get s (!i + 3));
          n := !n + uchar_len - 1;
          i := !i + uchar_len - 1
        | None ->
          let a = Char.code c in
          Bytes.unsafe_set s' !n '\\';
          incr n;
          Bytes.unsafe_set s' !n (Char.unsafe_chr (48 + (a / 100)));
          incr n;
          Bytes.unsafe_set s' !n (Char.unsafe_chr (48 + (a / 10 mod 10)));
          incr n;
          Bytes.unsafe_set s' !n (Char.unsafe_chr (48 + (a mod 10)))));
    incr n;
    incr i
  done
;;

(* Escape [s] if needed. *)
let escaped s =
  let n = quote_length s in
  if n = 0 || n > String.length s
  then (
    let s' = Bytes.create n in
    escape_to s ~dst:s' ~ofs:0;
    Bytes.unsafe_to_string s')
  else s
;;

(* Surround [s] with quotes, escaping it if necessary. *)
let quoted s =
  let len = String.length s in
  let n = quote_length s in
  let s' = Bytes.create (n + 2) in
  Bytes.unsafe_set s' 0 '"';
  if len = 0 || n > len
  then escape_to s ~dst:s' ~ofs:1
  else Bytes.blit_string ~src:s ~src_pos:0 ~dst:s' ~dst_pos:1 ~len;
  Bytes.unsafe_set s' (n + 1) '"';
  Bytes.unsafe_to_string s'
;;

let%expect_test "escaped - plain strings pass through unchanged" =
  let test s =
    let r = escaped s in
    Printf.printf "%S -> %S\n" s r
  in
  test "hello";
  test "foo_bar";
  test "123";
  test "a/b/c";
  [%expect
    {|
    "hello" -> "hello"
    "foo_bar" -> "foo_bar"
    "123" -> "123"
    "a/b/c" -> "a/b/c"
    |}]
;;

let%expect_test "escaped - special characters are escaped" =
  let test s =
    let r = escaped s in
    Printf.printf "%S -> %S\n" s r
  in
  test "has\"quote";
  test "back\\slash";
  test "new\nline";
  test "tab\there";
  test "car\rret";
  test "back\bspace";
  [%expect
    {|
    "has\"quote" -> "has\\\"quote"
    "back\\slash" -> "back\\\\slash"
    "new\nline" -> "new\\nline"
    "tab\there" -> "tab\\there"
    "car\rret" -> "car\\rret"
    "back\bspace" -> "back\\bspace"
    |}]
;;

let%expect_test "escaped - percent brace escaping" =
  let test s =
    let r = escaped s in
    Printf.printf "%S -> %S\n" s r
  in
  test "%{var}";
  test "100%";
  test "%%";
  test "%alone";
  [%expect
    {|
    "%{var}" -> "\\%{var}"
    "100%" -> "100%"
    "%%" -> "%%"
    "%alone" -> "%alone"
    |}]
;;

let%expect_test "escaped - empty string" =
  let r = escaped "" in
  Printf.printf "%S -> %S\n" "" r;
  [%expect {| "" -> "" |}]
;;

let%expect_test "escaped - non-ascii bytes are octal-escaped" =
  let test s =
    let r = escaped s in
    Printf.printf "%S -> %S\n" s r
  in
  test "\x00";
  test "\x01";
  test "\x7f";
  test "\xff";
  [%expect
    {|
    "\000" -> "\000"
    "\001" -> "\001"
    "\127" -> "\127"
    "\255" -> "\\255"
    |}]
;;

let%expect_test "escaped - valid utf8 passes through" =
  let test s =
    let r = escaped s in
    Printf.printf "%S -> %S\n" s r
  in
  (* 2-byte: é *)
  test "\xc3\xa9";
  (* 3-byte: € *)
  test "\xe2\x82\xac";
  (* 4-byte: 𝄞 *)
  test "\xf0\x9d\x84\x9e";
  [%expect
    {|
    "\195\169" -> "\195\169"
    "\226\130\172" -> "\226\130\172"
    "\240\157\132\158" -> "\240\157\132\158"
    |}]
;;

let%expect_test "quoted - wraps in double quotes" =
  let test s =
    let r = quoted s in
    Printf.printf "%S -> %s\n" s r
  in
  test "";
  test "hello";
  test "has space";
  test "has\"quote";
  test "new\nline";
  test "%{var}";
  [%expect
    {|
    "" -> ""
    "hello" -> "hello"
    "has space" -> "has space"
    "has\"quote" -> "has\"quote"
    "new\nline" -> "new\nline"
    "%{var}" -> "\%{var}"
    |}]
;;

let%expect_test "quote_length - matches actual escaped length" =
  let test s =
    let ql = quote_length s in
    let actual = String.length (escaped s) in
    if ql <> actual
    then Printf.printf "MISMATCH %S: quote_length=%d escaped_length=%d\n" s ql actual
  in
  test "";
  test "hello";
  test "has\"quote";
  test "new\nline";
  test "%{var}";
  test "\x00\xff";
  test "\xc3\xa9";
  test "\xe2\x82\xac";
  test "\xf0\x9d\x84\x9e";
  print_endline "all match";
  [%expect {| all match |}]
;;

let%expect_test "escaped - mixed content" =
  let test s =
    let r = escaped s in
    Printf.printf "%S -> %S\n" s r
  in
  test "hello\nworld\t!";
  test "say \"hi\" and \\go";
  test "%{x} is 100%";
  [%expect
    {|
    "hello\nworld\t!" -> "hello\\nworld\\t!"
    "say \"hi\" and \\go" -> "say \\\"hi\\\" and \\\\go"
    "%{x} is 100%" -> "\\%{x} is 100%"
    |}]
;;
