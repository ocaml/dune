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
  $ dune build -x esperanto ./cat.exe
  File "dune", line 1, characters 18-21:
  1 | (executable (name cat))
                        ^^^
  Error: Module "Cat" doesn't exist.
  [1]
