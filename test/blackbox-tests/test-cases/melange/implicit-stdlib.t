Depending on the Melange package does not make the implicit Stdlib available
when compiling a consumer in a sandbox.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.26)
  > (using melange 1.0)
  > (package (name melange))
  > EOF

Point the compiler at a local Stdlib, hiding the installed one.

  $ export REAL_MELC=$(command -v melc)
  $ unset MELANGELIB
  $ mkdir bin stdlib consumer
  $ cat > bin/melc <<'EOF'
  > #!/bin/sh
  > stdlib=$(dirname "$0")/../stdlib/.stdlib.objs/melange
  > if [ "$1" = --where ]; then
  >   printf '%s\n' "$stdlib"
  > else
  >   exec "$REAL_MELC" -nostdlib -I "$stdlib" "$@"
  > fi
  > EOF
  $ chmod +x bin/melc
  $ cat > dune <<'EOF'
  > (env (_ (binaries (bin/melc as melc))))
  > (alias
  >  (name bundle)
  >  (deps (package melange) consumer/.consumer.objs/melange/consumer.cmj))
  > EOF
  $ cat > stdlib/dune <<'EOF'
  > (library
  >  (name stdlib)
  >  (public_name melange)
  >  (modes melange)
  >  (flags :standard -nostdlib -nopervasives))
  > EOF
  $ echo 'let value = 42' > stdlib/stdlib.ml
  $ cat > consumer/dune <<'EOF'
  > (library
  >  (name consumer)
  >  (modes melange))
  > EOF
  $ echo 'let value = value' > consumer/consumer.ml
  $ echo 'val value : int' > consumer/consumer.mli

  $ dune build --sandbox=copy @bundle
  File "command line", line 1:
  Error: Unbound module Stdlib
  [1]
