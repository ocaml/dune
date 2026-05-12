open StdLabels
open Printf

(* This program performs version checking of the compiler and switches to the
   secondary compiler if necessary. The script should execute in OCaml 4.02! *)

let min_supported_natively = 4, 11, 0

let keep_generated_files =
  let anon s = raise (Arg.Bad (sprintf "don't know what to do with %s\n" s)) in
  let keep_generated_files = ref false in
  Arg.parse
    [ "-j", Arg.Int ignore, "JOBS Concurrency"
    ; "--verbose", Arg.Unit ignore, " Set the display mode"
    ; "--keep-generated-files", Arg.Set keep_generated_files, " Keep generated files"
    ; "--debug", Arg.Unit ignore, " Enable various debugging options"
    ; ( "--force-byte-compilation"
      , Arg.Unit ignore
      , " Force bytecode compilation even if ocamlopt is available" )
    ; "--static", Arg.Unit ignore, " Build a static binary"
    ; "--boot-dir", Arg.String (fun _ -> ()), " set the boot directory"
    ]
    anon
    "Usage: ocaml bootstrap.ml <options>\nOptions are:";
  !keep_generated_files
;;

let pps = "boot/pps"
let main = "boot/duneboot"
let modules = pps :: [ "boot/types"; "boot/libs" ]
let duneboot = ".duneboot"

let () =
  at_exit (fun () ->
    Array.iter (Sys.readdir "boot") ~f:(fun fn ->
      let fn = Filename.concat "boot" fn in
      if Filename.check_suffix fn ".cmi" || Filename.check_suffix fn ".cmo"
      then (
        try Sys.remove fn with
        | Sys_error _ -> ())));
  if not keep_generated_files
  then
    at_exit (fun () ->
      (try Sys.remove "boot/pps.ml" with
       | Sys_error _ -> ());
      Array.iter (Sys.readdir ".") ~f:(fun fn ->
        if
          String.length fn >= String.length duneboot
          && String.sub fn ~pos:0 ~len:(String.length duneboot) = duneboot
        then (
          try Sys.remove fn with
          | Sys_error _ -> ())))
;;

let runf fmt =
  ksprintf
    (fun cmd ->
       prerr_endline cmd;
       Sys.command cmd)
    fmt
;;

let exit_if_non_zero = function
  | 0 -> ()
  | n -> exit n
;;

let read_file fn =
  let ic = open_in_bin fn in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s
;;

let secondary_error () =
  Format.eprintf
    "@[%a@]@."
    Format.pp_print_text
    (let a, b, c = min_supported_natively in
     sprintf
       "The ocamlfind's secondary toolchain does not seem to be correctly installed.\n\
        Dune requires OCaml %d.%02d.%d or later to compile.\n\
        Please either upgrade your compiler or configure a secondary OCaml compiler (in \
        opam, this can be done by installing the ocamlfind-secondary package)."
       a
       b
       c);
  exit 2
;;

(* Locate the secondary compiler's bin directory via ocamlfind. We query
   ocamlfind rather than invoking [ocamlfind -toolchain secondary ocaml]
   directly because ocamlfind does not support the [ocaml] toplevel as a
   subcommand; it only wraps compiler tools like [ocamlc] and [ocamlopt]. *)
let find_secondary_bin_dir () =
  let output_fn, out = Filename.open_temp_file "duneboot" "ocamlfind-output" in
  let n =
    runf
      "ocamlfind -toolchain secondary query ocaml -format \"%%d\" >%s 2>/dev/null"
      output_fn
  in
  let bin_dir = String.trim (read_file output_fn) in
  close_out out;
  if n <> 0 || bin_dir = "" then secondary_error ();
  bin_dir
;;

let script chan =
  let pwd = Sys.getcwd () in
  let directive ~directive_name ~module_ =
    let fn = Filename.concat pwd (module_ ^ ".ml") in
    fprintf chan "#%s %S;;\n" directive_name fn
  in
  List.iter modules ~f:(fun module_ -> directive ~directive_name:"mod_use" ~module_);
  directive ~directive_name:"use" ~module_:main
;;

let () =
  let v = Scanf.sscanf Sys.ocaml_version "%d.%d.%d" (fun a b c -> a, b, c) in
  let compiler, ocamllex, which, secondary_lib =
    if v >= min_supported_natively
    then "ocaml", "ocamllex", None, None
    else (
      let bin_dir = find_secondary_bin_dir () in
      let lib_dir = Filename.concat (Filename.dirname bin_dir) "lib" in
      let compiler = Filename.concat bin_dir "ocaml" in
      let ocamllex = Filename.concat bin_dir "ocamllex" in
      compiler, ocamllex, Some "--secondary", Some lib_dir)
  in
  exit_if_non_zero (runf "%s -q -o %s %s" ocamllex (pps ^ ".ml") (pps ^ ".mll"));
  let script =
    let fname, out = Filename.open_temp_file duneboot "main" in
    script out;
    close_out out;
    fname
  in
  let args = List.tl (Array.to_list Sys.argv) in
  let args =
    match which with
    | None -> args
    | Some x -> x :: args
  in
  (* When using the secondary compiler, we must point it at its own lib
     directory (-I) and stub library directory (CAML_LD_LIBRARY_PATH) so it
     does not pick up the primary switch's incompatible libraries. *)
  let cmd =
    [ (match secondary_lib with
       | Some dir ->
         [ sprintf "CAML_LD_LIBRARY_PATH=%s" (Filename.concat dir "stublibs")
         ; compiler
         ; "-I"
         ; dir
         ]
       | None -> [ compiler ] @ if v >= (5, 0, 0) then [ "-I"; "+unix" ] else [])
    ; [ "unix.cma"; script ]
    ; args
    ]
    |> List.concat
    |> String.concat ~sep:" "
  in
  runf "%s" cmd |> exit_if_non_zero
;;
