open StdLabels
open Printf

(* This program performs version checking of the compiler and switches to the
   secondary compiler if necessary. The script should execute in OCaml 4.02! *)

let min_supported_natively = 4, 08, 0

let () =
  let anon s = raise (Arg.Bad (sprintf "don't know what to do with %s\n" s)) in
  Arg.parse
    [ "-j", Arg.Int ignore, "JOBS Concurrency"
    ; "--verbose", Arg.Unit ignore, " Set the display mode"
    ; "--debug", Arg.Unit ignore, " Enable various debugging options"
    ; ( "--force-byte-compilation"
      , Arg.Unit ignore
      , " Force bytecode compilation even if ocamlopt is available" )
    ; "--static", Arg.Unit ignore, " Build a static binary"
    ; "--boot-dir", Arg.String (fun _ -> ()), " set the boot directory"
    ]
    anon
    "Usage: ocaml bootstrap.ml <options>\nOptions are:"
;;

let main = "boot/duneboot"
let modules = [ "boot/types"; "boot/libs" ]

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
  let compiler, which =
    if v >= min_supported_natively
    then "ocaml", None
    else (
      let compiler = "ocamlfind -toolchain secondary ocaml" in
      let output_fn, out = Filename.open_temp_file "duneboot" "ocamlfind-output" in
      let n = runf "%s 2>%s" compiler output_fn in
      let s = read_file output_fn in
      close_out out;
      prerr_endline s;
      if n <> 0 || s <> ""
      then (
        Format.eprintf
          "@[%a@]@."
          Format.pp_print_text
          (let a, b, _ = min_supported_natively in
           sprintf
             "The ocamlfind's secondary toolchain does not seem to be correctly installed.\n\
              Dune requires OCaml %d.%02d or later to compile.\n\
              Please either upgrade your compile or configure a secondary OCaml compiler \
              (in opam, this can be done by installing the ocamlfind-secondary package)."
             a
             b);
        exit 2);
      compiler, Some "--secondary")
  in
  let script =
    let fname, out = Filename.open_temp_file "duneboot" "main" in
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
  let cmd =
    [ [ compiler ]
    ; (if v >= (5, 0, 0) then [ "-I"; "+unix" ] else [])
    ; [ "unix.cma"; script ]
    ; args
    ]
    |> List.concat
    |> String.concat ~sep:" "
  in
  runf "%s" cmd |> exit_if_non_zero
;;
