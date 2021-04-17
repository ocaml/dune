Test that files promoted to the source tree are writable by the user

Reproduction case for #3026

  $ cat > config <<EOF
  > (lang dune 2.1)
  > (cache enabled)
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 2.1)
  > EOF

  $ cat > dune <<EOF
  > (rule
  >  (mode promote)
  >  (action (with-stdout-to file (echo "Hello, world!\n"))))
  > EOF

Run Dune a first time to fill the cache, then delete the promoted file and run
Dune again. At the end of the first run, the file [_build/default/file] should
get deduplicated and so become read-only. As a result, on the second run the
promoted file will be copied from a read-only file. However, we still want the
user to be able to edit this file given that it is in the source tree, so Dune
should change the permission of this file.

We check that Dune does change the permission by echoing something into the file
after the second run.

  $ env XDG_RUNTIME_DIR=$PWD/.xdg-runtime \
  >     XDG_CACHE_HOME=$PWD/.xdg-cache \
  >   dune build --config-file=config file

  $ rm -f file

  $ env XDG_RUNTIME_DIR=$PWD/.xdg-runtime \
  >     XDG_CACHE_HOME=$PWD/.xdg-cache \
  >   dune build --config-file=config file

  $ cat file
  Hello, world!

  $ echo plop > file
