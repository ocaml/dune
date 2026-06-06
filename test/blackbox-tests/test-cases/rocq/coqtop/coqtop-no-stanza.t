Running the Coq Toplevel on a file that is not part of a stanza.

  $ cat >foo.v <<EOF
  > Definition mynat := nat.
  > EOF
  $ make_rocq_project 3.21 0.11
  $ dune rocq top foo.v || echo failed
  Error: Cannot find file: foo.v
  Hint: Is the file part of a stanza?
  Hint: Has the file been written to disk?
  failed
