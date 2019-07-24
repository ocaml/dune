  $ dune build
         hello alias default
  Hello: $TESTCASE_ROOT/a:/c

  $ mkdir sub
  $ cat > sub/dune-workspace <<EOF
  > (lang dune 1.11)
  > (context
  >  (default
  >    (paths (FOO a) (FOO b))))
  > EOF
  $ cat > sub/dune-project <<EOF
  > (lang dune 1.11)
  > EOF
  $ dune build --root sub
