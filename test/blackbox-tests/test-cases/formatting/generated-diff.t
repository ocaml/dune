Generated corrections are formatted as part of the diff action.

OCamlFormat receives the original source name and formats the correction in
place:

  $ mkdir ocaml
  $ cd ocaml
  $ make_dune_project 3.25
  $ echo 'let original = 0' > source.ml
  $ cat > dune <<'EOF'
  > (rule
  >  (alias generate)
  >  (action
  >   (progn
  >    (with-stdout-to source.ml.generated
  >     (echo "let   generated = 1"))
  >    (diff? source.ml source.ml.generated))))
  > EOF
  $ dune build @generate --auto-promote 2>&1 | grep 'fake ocamlformat is running'
  fake ocamlformat is running: "--impl" "--inplace" "--name" "source.ml" "source.ml.generated"
  [1]
  $ cat source.ml
  (* fake ocamlformat output *)

Projects older than Dune 3.25 retain the unformatted correction:

  $ cd ..
  $ mkdir old-project
  $ cd old-project
  $ make_dune_project 3.24
  $ echo 'let original = 0' > source.ml
  $ cat > dune <<'EOF'
  > (rule
  >  (alias generate)
  >  (action
  >   (progn
  >    (with-stdout-to source.ml.generated
  >     (echo "let   generated = 1"))
  >    (diff? source.ml source.ml.generated))))
  > EOF
  $ dune build @generate --auto-promote >/dev/null 2>&1
  [1]
  $ cat source.ml
  let   generated = 1

Generated dune files use Dune's built-in formatter. The correction is consumed
by [diff?] rather than exposed as an intermediate build target:

  $ cd ..
  $ mkdir dune-file
  $ cd dune-file
  $ make_dune_project 3.25
  $ echo '(rule  (alias generated))' > generated
  $ cat > dune <<'EOF'
  > (rule
  >  (alias generate)
  >  (action
  >   (progn
  >    (copy generated dune.generated)
  >    (diff? dune dune.generated))))
  > EOF
  $ dune build dune.generated
  Error: Don't know how to build dune.generated
  [1]
  $ dune build @generate --auto-promote >/dev/null 2>&1
  [1]
  $ cat dune
  (rule
   (alias generated))
