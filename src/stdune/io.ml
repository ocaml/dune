module P = Pervasives

let open_in ?(binary=true) p =
  let fn = Path.to_string p in
  if binary then P.open_in_bin fn else P.open_in fn

let open_out ?(binary=true) p =
  let fn = Path.to_string p in
  if binary then P.open_out_bin fn else P.open_out fn

let close_in  = close_in
let close_out = close_out

let with_file_in ?binary fn ~f =
  Exn.protectx (open_in ?binary fn) ~finally:close_in ~f

let with_file_out ?binary p ~f =
  Exn.protectx (open_out ?binary p) ~finally:close_out ~f

let with_lexbuf_from_file fn ~f =
  with_file_in fn ~f:(fun ic ->
    let lb = Lexing.from_channel ic in
    lb.lex_curr_p <-
      { pos_fname = Path.to_string fn
      ; pos_lnum  = 1
      ; pos_bol   = 0
      ; pos_cnum  = 0
      };
    f lb)

let input_lines =
  let rec loop ic acc =
    match input_line ic with
    | exception End_of_file -> List.rev acc
    | line ->
       loop ic (line :: acc)
  in
  fun ic -> loop ic []

let read_all ic =
  let len = in_channel_length ic in
  really_input_string ic len

let read_file fn = with_file_in fn ~f:read_all

let lines_of_file fn = with_file_in fn ~f:input_lines ~binary:false

let write_file fn data = with_file_out fn ~f:(fun oc -> output_string oc data)

let write_lines fn lines =
  with_file_out fn ~f:(fun oc ->
    List.iter ~f:(fun line ->
      output_string oc line;
      output_string oc "\n"
    ) lines
  )

let copy_channels =
  let buf_len = 65536 in
  let buf = Bytes.create buf_len in
  let rec loop ic oc =
    match input ic buf 0 buf_len with
    | 0 -> ()
    | n -> output oc buf 0 n; loop ic oc
  in
  loop

let copy_file ~src ~dst =
  with_file_in src ~f:(fun ic ->
    let perm = (Unix.fstat (Unix.descr_of_in_channel ic)).st_perm in
    Exn.protectx (P.open_out_gen
                    [Open_wronly; Open_creat; Open_trunc; Open_binary]
                    perm
                    (Path.to_string dst))
      ~finally:close_out
      ~f:(fun oc ->
        copy_channels ic oc))

(* TODO: diml: improve this *)
let compare_files fn1 fn2 = String.compare (read_file fn1) (read_file fn2)

let buf_len = 65_536

module Sexp = struct
  open Sexp

  let load path ~mode =
    with_file_in path ~f:(fun ic ->
      let state = Parser.create ~fname:(Path.to_string path) ~mode in
      let buf = Bytes.create buf_len in
      let rec loop stack =
        match input ic buf 0 buf_len with
        | 0 -> Parser.feed_eoi state stack
        | n -> loop (Parser.feed_subbytes state buf ~pos:0 ~len:n stack)
      in
      loop Parser.Stack.empty)

  let load_many_as_one path =
    match load path ~mode:Many with
    | [] -> Ast.List (Loc.in_file (Path.to_string path), [])
    | x :: l ->
      let last = Option.value (List.last l) ~default:x in
      let loc = { (Ast.loc x) with stop = (Ast.loc last).stop } in
      Ast.List (loc, x :: l)
end
