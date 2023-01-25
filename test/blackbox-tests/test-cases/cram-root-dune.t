  $ mkdir -p tests src
  $ cat > marc.opam << EOF
  > opam-version: "2.0"
  > EOF
  $ cat > dune-project << EOF
  > (lang dune 3.6)
  > (cram enable)
  > EOF
  $ cat > src/dune << EOF
  > (executable
  >   (name marc)
  >   (public_name marc)
  >   (package marc)
  > )
  > EOF
  $ echo 'let _ = Printf.printf "I am a dummy test!\n"' > src/marc.ml
  $ dune build
  $ dune exec -- marc
  I am a dummy test!

  $ cat > tests/marc.t << EOF
  >   $ marc
  >   I am a dummy test!
  > EOF
dun file is at the root
  $ echo "(cram (deps %{bin:marc}))" > dune

Check that cram test is ok
  $ dune runtest
Clean and try to run directly cram tests
  $ dune clean
  $ dune build @runtest
  File "tests/marc.t", line 1, characters 0-0:
  Error: Files _build/default/tests/marc.t and
  _build/default/tests/marc.t.corrected differ.
  [1]
Moving dune file into tests/ directory succeed
  $ mv dune tests/dune
  $ dune clean
  $ dune build @runtest
