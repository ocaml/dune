open Import

module Escape : sig
  val quoted : string -> string
end = struct
  let quote_length s =
    let n = ref 0 in
    let len = String.length s in
    for i = 0 to len - 1 do
      n
      := !n
         +
         match String.unsafe_get s i with
         | '\"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
         | '%' -> if i + 1 < len && s.[i + 1] = '{' then 2 else 1
         | ' ' .. '~' -> 1
         | _ -> 4
    done;
    !n
  ;;

  let escape_to s ~dst:s' ~ofs =
    let n = ref ofs in
    let escape c =
      Bytes.unsafe_set s' !n '\\';
      incr n;
      Bytes.unsafe_set s' !n c
    in
    let len = String.length s in
    for i = 0 to len - 1 do
      (match String.unsafe_get s i with
       | ('\"' | '\\') as c -> escape c
       | '\n' -> escape 'n'
       | '\t' -> escape 't'
       | '\r' -> escape 'r'
       | '\b' -> escape 'b'
       | '%' when i + 1 < len && s.[i + 1] = '{' -> escape '%'
       | ' ' .. '~' as c -> Bytes.unsafe_set s' !n c
       | c ->
         let a = Char.code c in
         escape (Char.unsafe_chr (48 + (a / 100)));
         incr n;
         Bytes.unsafe_set s' !n (Char.unsafe_chr (48 + (a / 10 mod 10)));
         incr n;
         Bytes.unsafe_set s' !n (Char.unsafe_chr (48 + (a mod 10))));
      incr n
    done
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
end

type t =
  | Quoted_string of string
  | List of t list

let rec to_string t =
  match t with
  | Quoted_string s -> Escape.quoted s
  | List l -> Printf.sprintf "(%s)" (List.map l ~f:to_string |> String.concat ~sep:" ")
;;
