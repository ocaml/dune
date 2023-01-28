Reproduce #6843

Cross compilation setup that causes dune to crash

  $ cat >dune-workspace <<EOF
  > (lang dune 3.6)
  > (context (default))
  > (context
  >  (default
  >   (name esperanto)
  >   (host default)))
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.6)
  > EOF
  $ cat >dune <<EOF
  > (executable (name cat))
  > EOF

  $ export OCAMLFIND_CONF=$PWD/etc/findlib.conf
  $ mkdir -p etc/findlib.conf.d
  $ touch etc/findlib.conf etc/findlib.conf.d/esperanto.conf
  $ dune build -x esperanto ./cat.exe 2>&1 | awk '/I must not/,/Only I will remain/'
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
