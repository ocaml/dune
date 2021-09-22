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

Currently, adding a new file while Dune is running in watching
corrupts Dune's internal state. For instance, here it seems to forget
how to build x.exe:

  $ touch b/blah

  $ build x.exe
  Failure
  $ _build/default/x.exe
  _build/default/x.exe: not found
  [127]

  $ stop_dune
  waiting for inotify sync
  waited for inotify sync
  Success, waiting for filesystem changes...
  waiting for inotify sync
  waited for inotify sync
  Error: Don't know how to build x.exe
  Had errors, waiting for filesystem changes...
