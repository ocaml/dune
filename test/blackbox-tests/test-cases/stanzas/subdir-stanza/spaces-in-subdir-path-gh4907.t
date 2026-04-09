Directories with spaces

  $ cat <<EOF >dune-project
  > (lang dune 3.0)
  > EOF
  $ cat <<EOF >dune
  > (subdir "foo bar" (rule (with-stdout-to foo (echo foo))))
  > EOF
  $ dune build
