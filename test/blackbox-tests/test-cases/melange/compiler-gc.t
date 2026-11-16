Melange tunes source compilation when its runtime supports small_heap_limit.
Explicit runtime settings and JavaScript emission are preserved.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using melange 1.0)
  > EOF
  $ unset OCAMLRUNPARAM CAMLRUNPARAM
  $ export REAL_MELC=$(command -v melc)
  $ export MANPATH=parent
  $ mkdir bin
  $ cat > bin/melc <<'EOF'
  > #!/bin/sh
  > if [ "$1" = --version ] && [ "${OCAMLRUNPARAM-}" = Xhelp ]; then
  >   if [ "${MELC_GC-yes}" = no ]; then exit 0; fi
  >   echo 'small_heap_limit (initial value 262144)' >&2
  >   if [ "${MELC_GC-yes}" = fail ]; then exit 1; fi
  >   exit 0
  > fi
  > previous=
  > for arg do
  >   if [ "$previous" = -o ]; then
  >     printf '%s: ocaml=%s legacy=%s man=%s\n' "${arg##*.}" \
  >       "${OCAMLRUNPARAM-unset}" "${CAMLRUNPARAM-unset}" "$MANPATH"
  >   fi
  >   previous=$arg
  > done
  > unset OCAMLRUNPARAM CAMLRUNPARAM
  > exec "$REAL_MELC" "$@"
  > EOF
  $ chmod +x bin/melc
  $ export PATH="$PWD/bin:$PATH"
  $ cat > dune <<'EOF'
  > (melange.emit (target output) (emit_stdlib false))
  > (env (_ (env-vars (MANPATH local))))
  > EOF
  $ touch main.ml main.mli
  $ build () {
  >   dune build --display quiet --cache=disabled \
  >     --build-dir "_build_$1" @all > log 2>&1 || { cat log; return 1; }
  >   sort -u log
  > }
  $ build defaults
  cmi: ocaml=Xsmall_heap_limit=16777216 legacy=unset man=local
  cmj: ocaml=Xsmall_heap_limit=16777216 legacy=unset man=local
  js: ocaml=unset legacy=unset man=local

  $ OCAMLRUNPARAM=s=123456 build explicit
  cmi: ocaml=s=123456 legacy=unset man=local
  cmj: ocaml=s=123456 legacy=unset man=local
  js: ocaml=s=123456 legacy=unset man=local
  $ CAMLRUNPARAM=s=123456 build legacy
  cmi: ocaml=unset legacy=s=123456 man=local
  cmj: ocaml=unset legacy=s=123456 man=local
  js: ocaml=unset legacy=s=123456 man=local
  $ OCAMLRUNPARAM= build empty
  cmi: ocaml= legacy=unset man=local
  cmj: ocaml= legacy=unset man=local
  js: ocaml= legacy=unset man=local
  $ CAMLRUNPARAM= build legacy-empty
  cmi: ocaml=unset legacy= man=local
  cmj: ocaml=unset legacy= man=local
  js: ocaml=unset legacy= man=local

Failed capability probes leave the compiler's defaults unchanged.

  $ MELC_GC=fail build failed-probe
  cmi: ocaml=unset legacy=unset man=local
  cmj: ocaml=unset legacy=unset man=local
  js: ocaml=unset legacy=unset man=local

One compiler wrapper can have different runtime capabilities per directory.

  $ mkdir unsupported
  $ cat > unsupported/dune <<'EOF'
  > (melange.emit (target output) (emit_stdlib false))
  > (env (_ (env-vars (MELC_GC no))))
  > EOF
  $ touch unsupported/main.ml unsupported/main.mli
  $ build mixed
  cmi: ocaml=Xsmall_heap_limit=16777216 legacy=unset man=local
  cmi: ocaml=unset legacy=unset man=local
  cmj: ocaml=Xsmall_heap_limit=16777216 legacy=unset man=local
  cmj: ocaml=unset legacy=unset man=local
  js: ocaml=unset legacy=unset man=local

Directory-local runtime settings are preserved too.

  $ cat > dune <<'EOF'
  > (melange.emit (target output) (emit_stdlib false))
  > (env
  >  (_
  >   (env-vars
  >    (OCAMLRUNPARAM s=456789)
  >    (CAMLRUNPARAM "")
  >    (MANPATH local))))
  > EOF
  $ build directory
  cmi: ocaml=s=456789 legacy= man=local
  cmj: ocaml=s=456789 legacy= man=local
  js: ocaml=s=456789 legacy= man=local
