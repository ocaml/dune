open StdLabels
open Printf

let ( ^/ ) = Filename.concat

(* Topoligically sorted *)
let modules =
  [ "Import"
  ; "Clflags"
  ; "Loc"
  ; "Meta_lexer"
  ; "Meta"
  ; "Bin"
  ; "Findlib"
  ; "Sexp"
  ; "Sexp_lexer"
  ; "Future"
  ; "Kind"
  ; "Values"
  ; "Rule"
  ; "Jbuild_interpret"
  ; "Main"
  ]

let lexers = [ "sexp_lexer"; "meta_lexer" ]

let path_sep =
  if Sys.win32 then
    ';'
  else
    ':'
;;

let split_path s =
  let rec loop i j =
    if j = String.length s then
      [String.sub s ~pos:i ~len:(j - i)]
    else if s.[j] = path_sep then
      String.sub s ~pos:i ~len:(j - i) :: loop (j + 1) (j + 1)
    else
      loop i (j + 1)
  in
  loop 0 0
;;

let path =
  match Sys.getenv "PATH" with
  | exception Not_found -> []
  | s -> split_path s
;;

let exe = if Sys.win32 then ".exe" else ""

let prog_not_found prog =
  eprintf "Program %s not found in PATH" prog;
  exit 2

type mode = Native | Byte

let best_prog dir prog =
  let fn = dir ^/ prog ^ ".opt" ^ exe in
  if Sys.file_exists fn then
    Some fn
    else
    let fn = dir ^/ prog ^ exe in
    if Sys.file_exists fn then
      Some fn
    else
      None

let find_prog prog =
  let rec search = function
    | [] -> None
    | dir :: rest ->
      match best_prog dir prog with
      | None -> search rest
      | Some fn -> Some (dir, fn)
  in
  search path

let get_prog dir prog =
  match best_prog dir prog with
  | None -> prog_not_found prog
  | Some fn -> fn

let count_newlines s =
  let newlines = ref 0 in
  String.iter s ~f:(function
    | '\n' -> incr newlines
    | _ -> ());
  !newlines

let read_file fn =
  let ic = open_in fn in
  let data = really_input_string ic (in_channel_length ic) in
  close_in ic;
  data

let generated_file = "jbuild.ml"

let generate_file_with_all_the_sources () =
  let oc = open_out "jbuild.ml" in
  let pos_in_generated_file = ref 1 in
  let pr fmt =
    ksprintf (fun s ->
      output_string oc s;
      output_char oc '\n';
      incr pos_in_generated_file)
      fmt
  in
  let dump fn =
    let s = read_file fn in
    pr "# 1 %S" fn;
    output_string oc s;
    let newlines = count_newlines s in
    let newlines =
      if s <> "" && s.[String.length s - 1] <> '\n' then begin
        output_char oc '\n';
        newlines + 1
      end else
        newlines
    in
    pos_in_generated_file := !pos_in_generated_file + newlines;
    pr "# %d %S" (!pos_in_generated_file + 1) generated_file
  in
  pr "module M : sig end = struct";
  List.iter modules ~f:(fun m ->
    let base = String.uncapitalize m in
    let mli = sprintf "src/%s.mli" base in
    let ml  = sprintf "src/%s.ml"  base in
    if Sys.file_exists mli then begin
      pr "module %s : sig" m;
      dump mli;
      pr "end = struct";
      dump ml;
      pr "end"
    end else begin
      pr "module %s = struct" m;
      dump ml;
      pr "end"
    end);
  pr "end";
  close_out oc

let exec fmt =
  ksprintf (fun cmd ->
    print_endline cmd;
    Sys.command cmd)
    fmt

let () =
  let bin_dir, mode, compiler =
    match find_prog "ocamlopt" with
    | Some (bin_dir, prog) -> (bin_dir, Native, prog)
    | None ->
      match find_prog "ocamlc" with
      | Some (bin_dir, prog) -> (bin_dir, Byte, prog)
      | None -> prog_not_found "ocamlc"
  in
  let ocamllex = get_prog bin_dir "ocamllex" in
  List.iter lexers ~f:(fun name ->
    let src = "src" ^/ name ^ ".mll" in
    let dst = "src" ^/ name ^ ".ml"  in
    let x = Sys.file_exists dst in
    let n = exec "%s %s" ocamllex src in
    if n <> 0 then exit n;
    if not x then
      at_exit (fun () -> try Sys.remove dst with _ -> ()));
  generate_file_with_all_the_sources ();
  let lib_ext =
    match mode with
    | Native -> "cmxa"
    | Byte   -> "cma"
  in
  exit (exec "%s -w -40 -o jbuild unix.%s %s" compiler lib_ext generated_file)
