open! Stdune

exception Fatal_error of string

exception Loc_error of Loc.t * string

let () =
  Printexc.register_printer (function
    | Loc_error (loc, s) -> Some (Format.asprintf "%a%s" Loc.print loc s)
    | _ -> None)

let fatalf ?loc fmt =
  Format.ksprintf (fun s ->
    match loc with
    | None -> raise (Fatal_error s)
    | Some loc -> raise (Loc_error (loc, s))
  ) fmt

exception Already_reported

let max_lines_to_print_in_full = 10

let context_lines = 2

let err_buf = Buffer.create 128
let err_ppf = Format.formatter_of_buffer err_buf
let kerrf fmt ~f =
  Format.kfprintf
    (fun ppf ->
       Format.pp_print_flush ppf ();
       let s = Buffer.contents err_buf in
       Buffer.clear err_buf;
       f s)
    err_ppf fmt

let die fmt =
  kerrf fmt ~f:(fun s -> raise (Fatal_error s))

let exnf t fmt =
  Format.pp_open_box err_ppf 0;
  Format.pp_print_as err_ppf 7 ""; (* "Error: " *)
  kerrf (fmt^^ "@]") ~f:(fun s -> Loc_error (t, s))

let fail t fmt =
  Format.pp_print_as err_ppf 7 ""; (* "Error: " *)
  kerrf fmt ~f:(fun s ->
    raise (Loc_error (t, s)))

let fail_lex lb fmt =
  fail (Loc.of_lexbuf lb) fmt

let fail_opt t fmt =
  match t with
  | None -> die fmt
  | Some t -> fail t fmt

let print ppf loc =
  Format.fprintf ppf "%a%a" Loc.print loc
    (Loc.pp_file_excerpt ~context_lines ~max_lines_to_print_in_full) loc

let warn t fmt =
  kerrf ~f:Console.print
    ("%a@{<warning>Warning@}: " ^^ fmt ^^ "@.") print t
