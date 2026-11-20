Cinaps corrections are formatted before being diffed starting with version 1.4
of the Cinaps extension, available in Dune 3.26.

OCaml sources are formatted when OCaml formatting is enabled:

  $ mkdir ocaml-1.4
  $ cd ocaml-1.4
  $ make_cinaps_project 3.26 1.4
  $ cat > dune <<'EOF'
  > (cinaps (files *.ml))
  > EOF
  $ cat > test.ml <<'EOF'
  > (*$ print_endline "\nlet   generated = 1" *)
  > (*$*)
  > EOF
  $ dune build @cinaps --auto-promote 2>&1 | grep 'fake ocamlformat is running'
  fake ocamlformat is running: "--impl" "--inplace" "--name" "test.ml" "test.ml.cinaps-corrected"
  [1]
  $ cat test.ml
  (* fake ocamlformat output *)
  $ dune build @cinaps

Older plugin versions retain unformatted corrections even with Dune 3.26:

  $ cd ..
  $ mkdir ocaml-1.3
  $ cd ocaml-1.3
  $ make_cinaps_project 3.26 1.3
  $ cat > dune <<'EOF'
  > (cinaps (files *.ml))
  > EOF
  $ cat > test.ml <<'EOF'
  > (*$ print_endline "\nlet   generated = 1" *)
  > (*$*)
  > EOF
  $ dune build @cinaps --auto-promote >/dev/null 2>&1
  [1]
  $ cat test.ml
  (*$ print_endline "\nlet   generated = 1" *)
  let   generated = 1
  (*$*)

Cinaps 1.4 requires Dune 3.26:

  $ cd ..
  $ mkdir unsupported
  $ cd unsupported
  $ make_cinaps_project 3.25 1.4
  $ dune build @cinaps
  File "dune-project", line 2, characters 14-17:
  2 | (using cinaps 1.4)
                    ^^^
  Error: Version 1.4 of the cinaps extension is not supported until version
  3.26 of the dune language.
  Supported versions of this extension in version 3.25 of the dune language:
  - 1.0 to 1.3
  [1]

Explicitly disabling formatting retains the generated contents:

  $ cd ..
  $ mkdir disabled
  $ cd disabled
  $ make_cinaps_project 3.26 1.4
  $ cat >> dune-project <<'EOF'
  > (formatting disabled)
  > EOF
  $ cat > dune <<'EOF'
  > (cinaps (files *.ml))
  > EOF
  $ cat > test.ml <<'EOF'
  > (*$ print_endline "\nlet   generated = 1" *)
  > (*$*)
  > EOF
  $ dune build @cinaps --auto-promote >/dev/null 2>&1
  [1]
  $ cat test.ml
  (*$ print_endline "\nlet   generated = 1" *)
  let   generated = 1
  (*$*)
