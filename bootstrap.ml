#warnings "-40";;
#load "unix.cma";;

module Array = ArrayLabels
module List  = ListLabels

module String = struct
  include StringLabels

  include struct
    [@@@warning "-3"]
    let capitalize_ascii   = String.capitalize
    let uncapitalize_ascii = String.uncapitalize
  end
end

open Printf

module String_set = Set.Make(String)

(* Modules overriden to bootstrap faster *)
let overridden =
  String_set.of_list
    [ "Glob_lexer"
    ]

let ( ^/ ) = Filename.concat

let protectx x ~finally ~f =
  match f x with
  | y           -> finally x; y
  | exception e -> finally x; raise e

let starts_with s ~prefix =
  let plen = String.length prefix in
  let slen = String.length s in
  slen >= plen && String.sub s ~pos:0 ~len:plen = prefix

let exec fmt =
  ksprintf (fun cmd ->
    print_endline cmd;
    Sys.command cmd)
    fmt

let path_sep =
  if Sys.win32 then
    ';'
  else
    ':'

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

let path =
  match Sys.getenv "PATH" with
  | exception Not_found -> []
  | s -> split_path s

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

let bin_dir, mode, compiler =
  match find_prog "ocamlopt" with
  | Some (bin_dir, prog) -> (bin_dir, Native, prog)
  | None ->
    match find_prog "ocamlc" with
    | Some (bin_dir, prog) -> (bin_dir, Byte, prog)
    | None -> prog_not_found "ocamlc"

let ocamllex = get_prog bin_dir "ocamllex"
let ocamldep = get_prog bin_dir "ocamldep"

let run_ocamllex name =
  let src = "src" ^/ name ^ ".mll" in
  let dst = "src" ^/ name ^ ".ml"  in
  let x = Sys.file_exists dst in
  let n = exec "%s -q %s" ocamllex src in
  if n <> 0 then exit n;
  if not x then
    at_exit (fun () -> try Sys.remove dst with _ -> ())

let modules =
  Sys.readdir "src"
  |> Array.fold_left ~init:[] ~f:(fun acc fn ->
    match String.rindex fn '.' with
    | exception Not_found -> acc
    | i ->
      let ext = String.sub fn ~pos:(i + 1) ~len:(String.length fn - i - 1) in
      match ext with
      | "ml" | "mll" ->
        let base = String.sub fn ~pos:0 ~len:i in
        let mod_name = String.capitalize_ascii base in
        if String_set.mem mod_name overridden then
          acc
        else begin
          if ext = "mll" then run_ocamllex base;
          String.capitalize_ascii base :: acc
        end
      | _ ->
        acc)
  |> String_set.of_list

let split_words s =
  let rec skip_blanks i =
    if i = String.length s then
      []
    else
      match s.[i] with
      | ' ' | '\t' -> skip_blanks (i + 1)
      | _ -> parse_word i (i + 1)
  and parse_word i j =
    if j = String.length s then
      [String.sub s ~pos:i ~len:(j - i)]
    else
      match s.[j] with
      | ' ' | '\t' -> String.sub s ~pos:i ~len:(j - i) :: skip_blanks (j + 1)
      | _ -> parse_word i (j + 1)
  in
  skip_blanks 0

let read_deps files =
  let ic =
    let cmd =
      sprintf "%s -modules %s"
        ocamldep (String.concat ~sep:" " files)
    in
    print_endline cmd;
    Unix.open_process_in cmd
  in
  let rec loop acc =
    match input_line ic with
    | exception End_of_file ->
      ignore (Unix.close_process_in ic);
      acc
    | line ->
      let i = String.index line ':' in
      let unit =
        String.sub line ~pos:0 ~len:i
        |> Filename.basename
        |> Filename.chop_extension
        |> String.capitalize_ascii
      in
      let deps =
        split_words (String.sub line ~pos:(i + 1)
                       ~len:(String.length line - (i + 1)))
        |> List.filter ~f:(fun m -> String_set.mem m modules)
      in
      loop ((unit, deps) :: acc)
  in
  loop []

let topsort deps =
  let n = List.length deps in
  let deps_by_module = Hashtbl.create n in
  List.iter deps ~f:(fun (m, deps) ->
    Hashtbl.add deps_by_module m deps);
  let not_seen = ref (List.map deps ~f:fst |> String_set.of_list) in
  let res = ref [] in
  let rec loop m =
    if String_set.mem m !not_seen then begin
      not_seen := String_set.remove m !not_seen;
      List.iter (Hashtbl.find deps_by_module m) ~f:loop;
      res := m :: !res
    end
  in
  while not (String_set.is_empty !not_seen) do
    loop (String_set.choose !not_seen)
  done;
  List.rev !res

let modules =
  let files =
    List.map (String_set.elements modules) ~f:(fun unit ->
      sprintf "src/%s.ml" (String.uncapitalize_ascii unit))
  in
  topsort (read_deps files)

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

let generated_file = "boot.ml"

let generate_file_with_all_the_sources () =
  let oc = open_out generated_file in
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
  let s = {|
module Jbuilder_re = struct
  module Re = struct
    type t = unit
    type re = unit
    let compile () = ()
    let execp _ _ = false
  end
end

module Glob_lexer = struct
  let parse_string _ = Error (0, "globs are not available during bootstrap")
end
|}
  in
  output_string oc s;
  pos_in_generated_file := !pos_in_generated_file + count_newlines s;
  List.iter modules ~f:(fun m ->
    let base = String.uncapitalize_ascii m in
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
  output_string oc "let () = Main.bootstrap ()\n";
  close_out oc

let () = generate_file_with_all_the_sources ()

let () =
  let lib_ext =
    match mode with
    | Native -> "cmxa"
    | Byte   -> "cma"
  in
  let n =
    protectx ()
      ~f:(fun () ->
        exec "%s -w -40 -o boot.exe unix.%s %s" compiler lib_ext generated_file)
      ~finally:(fun () ->
        try
          Array.iter (Sys.readdir ".") ~f:(fun fn ->
            if fn <> "boot.exe" && starts_with fn ~prefix:"boot." then
              Sys.remove fn)
        with _ ->
          ())
  in
  if n <> 0 then exit n
