We test that the calls to coqc -config in scope.ml is lazy enough that it will
not be called unless necessary.

We test this using a dummy coqc that will fail if it is called.

  $ cat > coqc << EOF
  > #!/bin/sh
  > exit 5
  > EOF
  $ chmod +x coqc

We add this to path:

  $ export PATH=$PWD:$PATH

Next we create an empty coq project:

  $ cat > dune-project << EOF
  > (lang dune 3.21)
  > (using rocq 0.11)
  > EOF

Finally we run dune build:

  $ dune build
