make_instrumentation_backends() {
  mkdir -p ppx trivial_ppx

  cat >ppx/dune-project <<'EOF'
(lang dune 2.7)

(package (name hello))
EOF

  cat >ppx/dune <<'EOF'
(library
 (name hello_ppx)
 (public_name hello.ppx)
 (kind ppx_rewriter)
 (ppx_runtime_libraries hello)
 (libraries ppxlib)
 (modules hello_ppx))

(library
 (public_name hello)
 (modules hello)
 (modes byte)
 (instrumentation.backend
  (ppx hello.ppx)))
EOF

  cat >ppx/hello.ml <<'EOF'
let hello s = print_endline (Printf.sprintf "Hello from %s!" s)
EOF

  cat >ppx/hello_ppx.ml <<'EOF'
open Ast_helper

let place = ref None
let file = ref None

let read_file () =
  match !file with
  | None -> "<none>"
  | Some s ->
    let ic = open_in s in
    (match input_line ic with
     | exception End_of_file ->
       close_in ic;
       "<none>"
     | s ->
       close_in ic;
       s)
;;

let impl str =
  let arg =
    match !place with
    | None -> Exp.ident (Location.mknoloc (Longident.Lident "__MODULE__"))
    | Some s -> Exp.constant (Const.string (Printf.sprintf "%s (%s)" s (read_file ())))
  in
  Str.eval
    (Exp.apply
       (Exp.ident
          (Location.mknoloc
             (Longident.Ldot
                ( { txt = Longident.Lident "Hello"; loc = Location.none }
                , { txt = "hello"; loc = Location.none } ))))
       [ Nolabel, arg ])
  :: str
;;

let () =
  Ppxlib.Driver.add_arg
    "-place"
    (Arg.String (fun s -> place := Some s))
    ~doc:"PLACE where to say hello from";
  Ppxlib.Driver.add_arg
    "-file"
    (Arg.String (fun s -> file := Some s))
    ~doc:"Add info from file"
;;

let () = Ppxlib.Driver.register_transformation_using_ocaml_current_ast ~impl "hello"
EOF

  cat >trivial_ppx/dune-project <<'EOF'
(lang dune 2.7)

(package (name trivial))
EOF

  cat >trivial_ppx/dune <<'EOF'
(library
 (name trivial_ppx)
 (public_name trivial.ppx)
 (kind ppx_rewriter)
 (libraries ppxlib)
 (modules trivial_ppx))
EOF

  cat >trivial_ppx/trivial_ppx.ml <<'EOF'
open Ppxlib

let () = Driver.register_transformation_using_ocaml_current_ast ~impl:Fun.id "trivial"
EOF
}

make_basic_instrumentation_project() {
  cat >dune-project <<'EOF'
(lang dune 2.7)
(wrapped_executables false)
EOF

  cat >dune <<'EOF'
(executable
 (name main)
 (modes byte)
 (modules main)
 (libraries mylib)
 (instrumentation (backend hello)))

(library
 (name mylib)
 (modes byte)
 (modules mylib)
 (instrumentation (backend hello)))
EOF

  cat >mylib.ml <<'EOF'
let f () = ()
EOF

  cat >main.ml <<'EOF'
let () = Mylib.f ()
EOF
}

make_argument_instrumentation_project() {
  cat >dune-project <<'EOF'
(lang dune 2.7)
(wrapped_executables false)
EOF

  cat >main.ml <<'EOF'
EOF

  cat >dune <<'EOF'
(executable
 (name main)
 (modes byte)
 (modules main)
 (preprocess (pps trivial.ppx))
 (instrumentation (backend hello -place Spain)))
EOF
}

make_dependency_instrumentation_project() {
  make_dune_project "${1:-3.0}"
  mkdir -p input
  cat >dune <<'EOF'
(data_only_dirs input)
(subdir input (rule (with-stdout-to input (echo "really"))))
(executable
 (name main)
 (modes byte)
 (modules main)
 (instrumentation (backend hello -place Spain -file input/input) (deps input/input)))
EOF
  cat >main.ml <<'EOF'
EOF
}
