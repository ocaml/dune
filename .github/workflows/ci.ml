open StdLabels

let skip_test =
  match Sys.getenv "SKIP_TEST" with
  | exception Not_found -> false
  | s -> bool_of_string s

let run cmd args =
  (* broken when arguments contain spaces but it's good enough for now. *)
  let cmd = String.concat " " (cmd :: args) in
  match Sys.command cmd with
  | 0 -> ()
  | n ->
    Printf.eprintf "'%s' failed with code %d" cmd n;
    exit n

let opam args = run "opam" args

let pin () =
  let packages =
    let packages = Sys.readdir "." |> Array.to_list in
    let packages =
      List.fold_left packages ~init:[] ~f:(fun acc fname ->
          if Filename.check_suffix fname ".opam" then
            Filename.chop_suffix fname ".opam" :: acc
          else
            acc)
    in
    if skip_test then
      List.filter packages ~f:(fun pkg -> pkg = "dune")
    else
      packages
  in
  List.iter packages ~f:(fun package ->
      opam [ "pin"; "add"; package ^ ".next"; "."; "--no-action" ])

let test () =
  if Sys.win32 then (
    opam [ "install"; "./dune-configurator.opam"; "--deps-only" ];
    run "make" [ "test-windows" ]
  ) else (
    opam [ "install"; "."; "--deps-only"; "--with-test" ];
    run "make" [ "dev-deps" ];
    run "make" [ "test" ]
  )

let () =
  match Sys.argv with
  | [| _; "pin" |] -> pin ()
  | [| _; "test" |] -> test ()
  | _ ->
    prerr_endline "Usage: ci.ml [pin | test]";
    exit 1
