Adding a lockdir should invalidate previously built artifacts and force a
rebuild. Here mypkg is first made available via the system (simulated through
OCAMLPATH) and main.exe is built against it. A lockdir is then introduced, and
main.exe should be rebuilt against the locked version rather than silently
keeping the artifact built against the system one. Set up a "system" install of
mypkg (simulating an opam switch install):

  $ mkdir mypkg-system
  $ cd mypkg-system
  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (package (name mypkg))
  > EOF
  $ cat > mypkg.ml <<EOF
  > let greeting = "hello from system mypkg"
  > EOF
  $ cat > dune <<EOF
  > (library
  >  (public_name mypkg))
  > EOF
  $ dune build @install
  $ dune install --prefix "$PWD/../prefix" > /dev/null 2>&1
  $ cd ..
  $ export OCAMLPATH="$PWD/prefix/lib"

Create a project that uses mypkg:

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (package
  >  (name myproject)
  >  (depends mypkg))
  > EOF
  $ cat > main.ml <<EOF
  > let () = print_endline Mypkg.greeting
  > EOF
  $ cat > dune <<EOF
  > (dirs (:standard \ mypkg-system \ mypkg-locked))
  > (executable
  >  (public_name main)
  >  (name main)
  >  (libraries mypkg))
  > EOF

Without a lockdir, the system version of mypkg is used:

  $ dune exec main
  hello from system mypkg

Prepare a different version of mypkg for the lockdir:

  $ mkdir mypkg-locked
  $ cd mypkg-locked
  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (package (name mypkg))
  > EOF
  $ cat > mypkg.ml <<EOF
  > let greeting = "hello from lockdir mypkg"
  > EOF
  $ cat > dune <<EOF
  > (library
  >  (public_name mypkg))
  > EOF
  $ cd ..

Introduce a lockdir pointing at the locked version of mypkg:

  $ make_lockdir
  $ make_lockpkg mypkg <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/mypkg-locked))
  > (build (run dune build --release --promote-install-file=true . @install))
  > EOF

After adding the lockdir, dune should rebuild main.exe using the locked version:

  $ dune exec main
  hello from lockdir mypkg
