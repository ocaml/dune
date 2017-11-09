[@@@ocaml.warning "-40"]

(* This module is here to build a version of jbuilder that is capable of
 * building itself. It accomplishes this by concatenating all its source files
 * into a single .ml file and simply compiling it. The source code of the
 * vendored libraries are omitted, being replaced by stubs, just to speed up
 * the bootstrapping process. This is possible because the features used in
 * jbuilder's jbuild files use a minimal set of features that do not actually
 * hit codepaths in which the vendored libraries are used. In order for this to
 * continue to work, jbuild files in the jbuilder repository should not use
 * globs. *)

module Array = ArrayLabels
module List  = ListLabels

module String = struct
  include StringLabels

  include struct
    [@@@warning "-3"]
    let capitalize_ascii   = String.capitalize
    let uncapitalize_ascii = String.uncapitalize
  end

  let break s ~pos =
    (sub s ~pos:0 ~len:pos,
     sub s ~pos ~len:(String.length s - pos))
end

let dirs =
  [ "vendor/boot"
  ; "src"
  ]

open Printf

module String_set = Set.Make(String)
module String_map = Map.Make(String)

let () =
  match Sys.getenv "OCAMLPARAM" with
  | s -> Printf.eprintf "OCAMLPARAM is set to %S\n%!" s
  | exception Not_found -> ()

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
  match find_prog "ocamlc" with
  | None -> prog_not_found "ocamlc"
  | Some (bin_dir, prog) ->
    match best_prog bin_dir "ocamlopt" with
    | Some prog -> (bin_dir, Native, prog)
    | None -> (bin_dir, Byte, prog)

let ocamllex = get_prog bin_dir "ocamllex"
let ocamldep = get_prog bin_dir "ocamldep"

let run_ocamllex src =
  let dst = String.sub src ~pos:0 ~len:(String.length src - 1) in
  let x = Sys.file_exists dst in
  let n = exec "%s -q %s" (Filename.quote ocamllex) src in
  if n <> 0 then exit n;
  if not x then
    at_exit (fun () -> try Sys.remove dst with _ -> ());
  dst

type module_files =
  { impl : string
  ; intf : string option
  }

(* Map from module names to ml/mli filenames *)
let modules =
  let files_of dir =
    Sys.readdir dir |> Array.to_list |> List.map ~f:(Filename.concat dir)
  in
  let impls, intfs =
    List.map dirs ~f:files_of
    |> List.concat
    |> List.fold_left ~init:(String_map.empty, String_map.empty)
         ~f:(fun ((impls, intfs) as acc) fn ->
           let base = Filename.basename fn in
           match String.index base '.' with
           | exception Not_found -> acc
           | i ->
             let base, ext = String.break base i in
             let is_boot, ext =
               match String.rindex ext '.' with
               | exception Not_found -> (false, ext)
               | i ->
                 let a, b = String.break ext i in
                 if a = ".boot" then
                   (true, b)
                 else
                   (false, ext)
             in
             match ext with
             | ".ml" | ".mll" ->
               let mod_name = String.capitalize_ascii base in
               if is_boot || not (String_map.mem mod_name impls) then
                 let fn =
                   if ext = ".mll" then lazy (run_ocamllex fn) else lazy fn
                 in
                 (String_map.add mod_name fn impls, intfs)
               else
                 acc
             | ".mli" ->
               let mod_name = String.capitalize_ascii base in
               if is_boot || not (String_map.mem mod_name intfs) then
                 (impls, String_map.add mod_name fn intfs)
               else
                 acc
             | _ -> acc)
  in
  String_map.merge
    (fun _ impl intf ->
       match impl with
       | None -> None
       | Some impl -> Some { impl = Lazy.force impl; intf })
    impls intfs

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

let read_lines fn =
  let ic = open_in fn in
  let rec loop ic acc =
    match try Some (input_line ic) with End_of_file -> None with
    | Some line -> loop ic (line :: acc)
    | None -> List.rev acc
  in
  let lines = loop ic [] in
  close_in ic;
  lines

let read_deps files =
  let out_fn = "boot-depends.txt" in
  at_exit (fun () -> Sys.remove out_fn);
  let n =
    exec "%s -modules %s > %s"
      (Filename.quote ocamldep) (String.concat ~sep:" " files) out_fn
  in
  if n <> 0 then exit n;
  List.map (read_lines out_fn) ~f:(fun line ->
    let i = String.index line ':' in
    let unit =
      String.sub line ~pos:0 ~len:i
      |> Filename.basename
      |> (fun s -> String.sub s ~pos:0 ~len:(String.index s '.'))
      |> String.capitalize_ascii
    in
    let deps =
      split_words (String.sub line ~pos:(i + 1)
                     ~len:(String.length line - (i + 1)))
      |> List.filter ~f:(fun m -> String_map.mem m modules)
    in
    (unit, deps))

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

let topsorted_module_names =
  let files =
    List.map (String_map.bindings modules) ~f:(fun (_, x) -> x.impl)
  in
  topsort (read_deps files)

let count_newlines s =
  let newlines = ref 0 in
  String.iter s ~f:(function
    | '\n' -> incr newlines
    | _ -> ());
  !newlines

let read_file fn =
  let ic = open_in_bin fn in
  let data = really_input_string ic (in_channel_length ic) in
  close_in ic;
  data

let generated_file = "boot.ml"

let generate_file_with_all_the_sources () =
  let oc = open_out_bin generated_file in
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
  List.iter topsorted_module_names ~f:(fun m ->
    let { impl; intf } =
      try
        String_map.find m modules
      with Not_found ->
        Printf.ksprintf failwith "module not found: %s" m
    in
    match intf with
    | Some intf ->
      pr "module %s : sig" m;
      dump intf;
      pr "end = struct";
      dump impl;
      pr "end"
    | None ->
      pr "module %s = struct" m;
      dump impl;
      pr "end");
  output_string oc "let () = Main.bootstrap ()\n";
  close_out oc

let () = generate_file_with_all_the_sources ()

let cleanup ~keep_ml_file =
  try
    Array.iter (Sys.readdir ".") ~f:(fun fn ->
      if fn <> "boot.exe" &&
         starts_with fn ~prefix:"boot." &&
         (fn <> "boot.ml" || not keep_ml_file) then
        Sys.remove fn)
  with _ ->
    ()

let () =
  let lib_ext =
    match mode with
    | Native -> "cmxa"
    | Byte   -> "cma"
  in
  let n =
    try exec "%s -w -40 -o boot.exe unix.%s %s"
          (Filename.quote compiler) lib_ext generated_file
    with e -> cleanup ~keep_ml_file:true; raise e
  in
  cleanup ~keep_ml_file:(n <> 0);
  if n <> 0 then exit n
