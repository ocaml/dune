open Stdune

let debug_mode = ref true

let cannot_open s msg =
  User_error.raise Pp.[ Pp.O.(text s ++ text ":" ++ space ++ text msg) ]

let no_file_provided () =
  User_error.raise Pp.[ text "No file provided. Please provide a file." ]

let too_many_files_provided () =
  User_error.raise
    Pp.[ text "Too many files\n  provided. Please provide only a single file." ]

let find_dependencies f =
  let chan = try open_in f with Sys_error msg -> cannot_open f msg in
  let buf = Lexing.from_channel chan in
  Lexing.set_filename buf f;
  let toks = Coqmod.lexbuf buf in
  close_in chan;
  toks |> Coqmod.to_csexp |> Csexp.to_channel stdout

let main () =
  let usage_msg = "coqmod - A simple module lexer for Coq" in
  let files = ref [] in
  let anon_fun f = files := f :: !files in
  let speclist =
    [ ("--debug", Arg.Set debug_mode, "Output debugging information") ]
  in
  let () = Arg.parse speclist anon_fun usage_msg in
  match !files with
  | [] -> no_file_provided ()
  | [ file ] -> find_dependencies file
  | _ -> too_many_files_provided ()

let () =
  try main ()
  with exn -> (
    match exn with
    | User_error.E err -> Dune_console.print_user_message err
    | _ -> raise exn)
