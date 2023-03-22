Show an error when `(ppx ...)` appears in any position other than the last

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > EOF
  $ cat > dune <<EOF
  > (executable
  >  (name foo)
  >  (modules foo)
  >  (preprocess
  >   (action
  >    (progn
  >     (with-stdout-to %{input-file}.pp.1
  >      (progn
  >       (run cat %{input-file})
  >       (echo "let () = print_endline \"one more line\"")))
  >     (ppx (some_ppx) %{input-file}.pp.1)
  >     (progn
  >      (run cat %{input-file})
  >      (echo "let () = print_endline \"last line\""))))))
  > EOF
  $ touch foo.ml

Not available until dune lang 3.8
  $ dune exec ./foo.exe
  File "dune", line 11, characters 4-39:
  11 |     (ppx (some_ppx) %{input-file}.pp.1)
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'ppx' is only available since version 3.8 of the dune language. Please
  update your dune-project file to have (lang dune 3.8).
  [1]

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > EOF
  $ dune exec ./foo.exe
  File "dune", line 11, characters 9-19:
  11 |     (ppx (some_ppx) %{input-file}.pp.1)
                ^^^^^^^^^^
  Error: The (ppx ..) action is currently only supported in the last position.
  [1]

Using the ppx action within a rule is not yet possible

  $ cat > dune <<EOF
  > (rule (with-stdout-to foo.ml (echo "let () = print_endline \"first line\"")))
  > (rule
  >  (target foo.pp.ml)
  >  (deps foo.ml)
  >  (action
  >   (progn
  >    (with-stdout-to %{deps}.pp.1
  >     (progn
  >      (run cat %{deps})
  >      (echo "let () = print_endline \"second line\"")))
  >    (ppx (some_ppx) %{deps}.pp.1))))
  > EOF
  $ dune build ./foo.pp.ml
  File "dune", line 11, characters 8-18:
  11 |    (ppx (some_ppx) %{deps}.pp.1))))
               ^^^^^^^^^^
  Error: The `ppx' action isn't currently available in (rule ...) stanzas.
  [1]
