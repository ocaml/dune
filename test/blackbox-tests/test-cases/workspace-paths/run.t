  $ dune build
         hello alias default
  Hello: $TESTCASE_ROOT/a:/c

  $ mkdir sub
  $ cat > sub/dune-workspace <<EOF
  > (lang dune 1.12)
  > (context
  >  (default
  >    (env (_ (paths (FOO a) (FOO b))))))
  > EOF
  $ cat > sub/dune-project <<EOF
  > (lang dune 1.12)
  > EOF
  $ dune build --root sub
  Entering directory 'sub'
  File "dune-workspace", line 4, characters 27-30:
  4 |    (env (_ (paths (FOO a) (FOO b))))))
                                 ^^^
  Error: the variable "FOO" can appear at most once in this stanza.
  [1]
