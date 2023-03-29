This is a bug occur that occurs when we're running dune in watch mode and
adding . to $CWD

Reproduce #6907

  $ . ./helpers.sh

  $ export PATH=.:$PATH
  $ echo "(lang dune 2.0)" > dune-project

  $ start_dune

  $ cat > x <<EOF
  > original-contents
  > EOF

  $ cat >dune <<EOF
  > (rule (with-stdout-to y (echo %{read:x})))
  > EOF

  $ build y
  Success

  $ touch x

  $ build y
  Success

  $ stop_dune
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
