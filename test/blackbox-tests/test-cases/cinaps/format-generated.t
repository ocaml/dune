Cinaps uses the project's generated correction formatting setting.

Opting in formats OCaml corrections without a new Cinaps extension version:

  $ mkdir enabled
  $ cd enabled
  $ make_cinaps_project 3.26 1.3
  $ echo '(format_generated_corrections true)' >> dune-project
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

Without the setting, corrections stay unformatted even with Dune 3.26:

  $ cd ..
  $ mkdir default
  $ cd default
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

Disabling formatting takes precedence over opting in to correction formatting:

  $ cd ..
  $ mkdir disabled
  $ cd disabled
  $ make_cinaps_project 3.26 1.3
  $ cat >> dune-project <<'EOF'
  > (format_generated_corrections true)
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
