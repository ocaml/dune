Demonstrate a dependency cycle between library modules field and a rule that
generates them dynamically in the same directory

  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > EOF

  $ mkdir gen
  $ cat > gen/dune << EOF
  > (executable
  >  (name gen)
  >  (libraries y))
  > (rule
  >  (with-stdout-to dyn_modules (run ./gen.exe)))
  > EOF
  $ cat > gen/gen.ml <<EOF
  > let () =
  >   Format.eprintf "generating. %s from 'y'" Y.x;
  >   Format.printf "foo"
  > EOF
  $ touch foo.ml
  $ cat > dune << EOF
  > (library
  >  (name x)
  >  (modules %{read:gen/dyn_modules}))
  > (library
  >  (name y)
  >  (modules y))
  > EOF
  $ cat > y.ml <<EOF
  > let x = "hello"
  > EOF
  $ dune build ./gen/dyn_modules
  Error: Dependency cycle between:
     _build/default/gen/dyn_modules
  -> %{read:gen/dyn_modules} at dune:3
  -> (modules) field at dune:1
  -> _build/default/gen/.merlin-conf/exe-gen
  -> _build/default/gen/gen.exe
  -> _build/default/gen/dyn_modules
  [1]
