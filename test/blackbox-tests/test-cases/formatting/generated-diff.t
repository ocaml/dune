Generated corrections are formatted as part of the diff action.

The formatter's inputs must be available before the optional diff checks
whether a correction exists:

  $ mkdir custom
  $ cd custom
  $ make_dune_project 3.26
  $ cat >> dune-project <<'EOF'
  > (formatting (enabled_for custom))
  > (dialect
  >  (name custom)
  >  (implementation
  >   (extension custom)
  >   (format (cat %{input-file} suffix))))
  > EOF
  $ echo 'unformatted' > source.custom
  $ cat > dune <<'EOF'
  > (rule
  >  (alias missing-correction)
  >  (action (diff? source.custom source.custom.generated)))
  > EOF
  $ dune build @missing-correction
  Error: No rule found for suffix
  -> required by alias missing-correction in dune:1
  [1]
  $ cd ..

OCamlFormat receives the original source name and formats the correction in
place:

  $ mkdir ocaml
  $ cd ocaml
  $ make_dune_project 3.26
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

Projects older than Dune 3.26 retain the unformatted correction:

  $ cd ..
  $ mkdir old-project
  $ cd old-project
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
  $ dune build @generate --auto-promote >/dev/null 2>&1
  [1]
  $ cat source.ml
  let   generated = 1

Action-based dialect formatters write atomically back to the correction:

  $ cd ../custom
  $ dune_cmd subst 'cat [^)]*' 'run ocamlformat %{input-file}' dune-project
  $ cat >> dune <<'EOF'
  > (rule
  >  (alias generate)
  >  (action
  >   (progn
  >    (with-stdout-to source.custom.generated
  >     (echo "generated"))
  >    (diff? source.custom source.custom.generated))))
  > EOF
  $ dune build @generate --auto-promote 2>&1 | grep 'fake ocamlformat is running'
  fake ocamlformat is running: "./source.custom.generated"
  [1]
  $ cat source.custom
  (* fake ocamlformat output *)

The formatter reads the intermediate correction without requiring a rule for
it, but Dune still builds the formatter's other inputs:

  $ dune_cmd subst 'run ocamlformat [^)]*' 'cat %{input-file} suffix' dune-project
  $ cat >> dune <<'EOF'
  > (rule (action (with-stdout-to suffix (echo " with suffix"))))
  > EOF
  $ dune build @generate --auto-promote > output 2>&1
  [1]
  $ cat source.custom
  generated with suffix

Generated dune files use Dune's built-in formatter. The correction is consumed
by [diff?] rather than exposed as an intermediate build target:

  $ cd ..
  $ mkdir dune-file
  $ cd dune-file
  $ make_dune_project 3.26
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

A directory correction can replace a source file without running a file formatter:

  $ cd ..
  $ mkdir directory-replacement
  $ cd directory-replacement
  $ make_dune_project 3.26
  $ echo 'original' > source.ml
  $ cat > dune <<'EOF'
  > (rule
  >  (alias generate)
  >  (action
  >   (progn
  >    (system "mkdir correction; echo generated > correction/contents")
  >    (diff? source.ml correction))))
  > EOF
  $ dune build @generate --auto-promote > output 2>&1
  [1]
  $ grep 'fake ocamlformat' output
  [1]
  $ test -d source.ml && cat source.ml/contents
  generated
