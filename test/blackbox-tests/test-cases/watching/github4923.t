Reproduction case for #4923: weird things observed when we add/remove
a file in watch mode.

  $ . ./helpers.sh

  $ echo '(lang dune 3.0)' > dune-project
  $ mkdir a b
  $ cat > a/dune <<EOF
  > (library (name a))
  > EOF
  $ cat > b/dune <<EOF
  > (library (name b))
  > EOF
  $ touch a/a.ml b/b.ml
  $ cat > dune <<EOF
  > (executable (name x) (libraries b))
  > EOF
  $ echo 'print_endline "Hello, world!"' > x.ml

  $ start_dune

  $ build x.exe
  Success
  $ _build/default/x.exe
  Hello, world!

In the past, adding a new file while Dune was running in watching mode
was corrupting Dune's internal state. For instance, here it no longer
knew how to build x.exe. This is now fixed.

  $ touch b/blah

  $ build x.exe
  Success
  $ _build/default/x.exe
  Hello, world!

  $ stop_dune
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
