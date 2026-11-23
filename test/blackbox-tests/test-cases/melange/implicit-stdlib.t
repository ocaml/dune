Melange compilation should track its implicit Stdlib, including when a bundle
depends on the Melange package.

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
  > case "${MELANGELIB-}" in
  >   "") stdlib=$(dirname "$0")/../stdlib/.stdlib.objs/melange ;;
  >   /*) stdlib=$MELANGELIB ;;
  >   *) stdlib=$(dirname "$0")/$MELANGELIB ;;
  > esac
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

Building the consumer alone should first build the implicit Stdlib.

  $ dune build consumer/.consumer.objs/melange/consumer.cmj

The implicit compilation inputs must also be available in a fresh copy sandbox.

  $ dune build --build-dir _build_bundle --sandbox=copy @bundle

Prebuild Stdlib and disable sandboxing to check incremental dependencies.

  $ dune build stdlib/.stdlib.objs/melange/stdlib.cmj
  $ dune build --sandbox=none consumer/.consumer.objs/melange/consumer.cmj

Changing a Stdlib implementation should rebuild the consumer even when
the interface remains unchanged.

  $ echo 'let value = 43' > stdlib/stdlib.ml
  $ dune build --sandbox=none consumer/.consumer.objs/melange/consumer.cmj
  $ dune trace cat | jq_dune -s '[.[] | processesBrief
  >   | select(.prog == "melc" and (.args | index("-o")))] | length'
  2

Absolute and compiler-relative MELANGELIB overrides also identify build inputs.

  $ MELANGELIB="$PWD/_build_absolute/default/stdlib/.stdlib.objs/melange" \
  > dune build --build-dir _build_absolute \
  >   consumer/.consumer.objs/melange/consumer.cmj
  $ MELANGELIB=../stdlib/.stdlib.objs/melange \
  > dune build --build-dir _build_relative --sandbox=copy \
  >   consumer/.consumer.objs/melange/consumer.cmj

An explicit library dependency remains valid.

  $ cat > consumer/dune <<'EOF'
  > (library
  >  (name consumer)
  >  (modes melange)
  >  (libraries melange))
  > EOF
  $ dune build --build-dir _build_bundle --sandbox=copy @bundle

Libraries compiled with -nostdlib do not build or depend on the implicit Stdlib.

  $ cat > consumer/dune <<'EOF'
  > (library
  >  (name consumer)
  >  (modes melange)
  >  (melange.flags :standard -nostdlib -nopervasives))
  > EOF
  $ echo 'let value = 0' > consumer/consumer.ml
  $ dune build --build-dir _build_nostdlib \
  >   consumer/.consumer.objs/melange/consumer.cmj
  $ test ! -e _build_nostdlib/default/stdlib/.stdlib.objs/melange/stdlib.cmi
