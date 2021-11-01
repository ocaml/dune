Duplicate hello.exe before writing to it, as modifying a produced file
is prohibited.

  $ cp bin/hello.exe.source bin/hello.exe
  $ chmod u+w bin/hello.exe

  $ dune build
  Hello: $TESTCASE_ROOT/a:/c

  $ mkdir sub
  $ cat > sub/dune-workspace <<EOF
  > (lang dune 1.12)
  > (context
  >  (default
  >    (paths (FOO a) (FOO b))))
  > EOF
  $ cat > sub/dune-project <<EOF
  > (lang dune 1.12)
  > EOF
  $ dune build --root sub
  Entering directory 'sub'
  File "dune-workspace", line 4, characters 19-22:
  4 |    (paths (FOO a) (FOO b))))
                         ^^^
  Error: the variable "FOO" can appear at most once in this stanza.
  [1]
