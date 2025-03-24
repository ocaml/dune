A package that installs itself into the ocaml stdlib should work.

  $ . ./helpers.sh

  $ mkdir nondune
  $ cd nondune
  $ cat > nondune.ml <<EOF
  > let main () = print_endline "Nondune"
  > let () = main ()
  > EOF
  $ ocamlc -c nondune.ml
  $ ocamlc -o nondune.cma -a nondune.cmo
  $ cat > nondune.install <<EOF
  > lib_root: [
  >   "nondune.cma" {"ocaml/nondune.cma"}
  >   "nondune.cmi" {"ocaml/nondune.cmi"}
  > ]
  > lib: [
  >   "META" {"META"}
  > ]
  > EOF
  $ cat > META <<EOF
  > directory = "^"
  > archive(byte) = "nondune.cma"
  > EOF
  $ cat > nondune.opam <<EOF
  > opam-version: "2.0"
  > build: [
  >  [true]
  > ]
  > EOF
  $ cd ..

With this project set up, lets depend on it.

  $ mkdir foo
  $ cd foo
  $ mkrepo
  $ solve_project <<EOF
  > (lang dune 3.15)
  > (pin
  >  (url "file://$PWD/../nondune")
  >  (package (name nondune)))
  > (package
  >  (name foo)
  >  (depends nondune))
  > EOF
  Solution for dune.lock:
  - nondune.dev
  $ cat > dune <<EOF
  > (executable
  >  (name foo)
  >  (modules foo)
  >  (libraries nondune)
  >  (modes byte))
  > EOF
  $ cat > foo.ml <<EOF
  > let () = Nondune.main ()
  > EOF
  $ dune exec ./foo.exe 2>&1 | grep -o "Unbound module Nondune"
  Unbound module Nondune
