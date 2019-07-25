  $ dune build
         hello alias default
  Hello: $TESTCASE_ROOT/a:/c

  $ mkdir sub
  $ cat > sub/dune-workspace <<EOF
  > (lang dune 1.12)
  > (context
  >  (default
  >    (paths (FOO a) (FOo b))))
  > EOF
  $ cat > sub/dune-project <<EOF
  > (lang dune 1.12)
  > EOF
  $ dune build --root sub
  Entering directory 'sub'
  File "dune-workspace", line 4, characters 19-22:
  4 |    (paths (FOO a) (FOo b))))
                         ^^^
  Error: the variable "FOo" can appear at most once in this stanza.
  [1]
