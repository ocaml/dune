Cinaps corrections are formatted before being diffed starting in Dune 3.25.

OCaml sources are formatted when OCaml formatting is enabled:

  $ mkdir ocaml-3.25
  $ cd ocaml-3.25
  $ make_cinaps_project 3.25 1.0
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

The behavior is gated by the Dune language version:

  $ cd ..
  $ mkdir ocaml-3.24
  $ cd ocaml-3.24
  $ make_cinaps_project 3.24 1.0
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

Explicitly disabling formatting retains the generated contents:

  $ cd ..
  $ mkdir disabled
  $ cd disabled
  $ make_cinaps_project 3.25 1.0
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
