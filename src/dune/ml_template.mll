{
  open! Stdune
  type chunk =
    | Text of string
    | Section of { name : string }

  type t = chunk list

  let commit_text acc buf =
    let text = Buffer.contents buf in
    Buffer.clear buf;
    if text = "" then
      acc
    else
      Text text :: acc

  let make acc buf =
    List.rev (commit_text acc buf)

  let new_var acc name =
    Section { name } :: acc
}

let blank = [' ' '\t' '\n' '\012']

let marker_start = "(*$" blank*

let marker_end = blank* "$*)"

let var = ['0'-'9' 'a'-'z' 'A'-'Z' '_' '-' '.']+

rule template acc buf = parse
 | marker_start (var as name) marker_end
   { let acc = commit_text acc buf in
     section acc buf name lexbuf
   }
 | _ as c
  { Buffer.add_char buf c;
    template acc buf lexbuf
  }
 | eof { make acc buf }
and section acc buf name = parse
 | marker_start (var as name') marker_end
   { if name' = "end" then
       let acc = new_var acc name in
       template acc buf lexbuf
     else
       failwith "nesting sections not allowed"
   }
 | _ { section acc buf name lexbuf }
 | eof { failwith ("Unterminated section " ^ name) }

{
  let of_string s =
    let t = Lexing.from_string s in
    template [] (Buffer.create 512) t

  let substitute_all t ~f =
    let b = Buffer.create 1024 in
    List.iter t ~f:(function
        | Text s -> Buffer.add_string b s
        | Section { name } -> Buffer.add_string b (f name));
    Buffer.contents b
}
