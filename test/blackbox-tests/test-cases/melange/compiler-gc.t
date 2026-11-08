Melange compilation preserves explicit runtime settings and directory
environments.
The wrapper simulates runtime capabilities independently of compiler versions.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using melange 1.0)
  > EOF
  $ unset OCAMLRUNPARAM CAMLRUNPARAM
  $ export REAL_MELC=$(command -v melc)
  $ export REAL_OCAML_BIN=$(dirname "$(command -v ocamlc)")
  $ export PROBE_LOG="$PWD/probe.log"
  $ export MANPATH=parent
  $ mkdir bin
  $ cat > bin/melc <<'EOF'
  > #!/bin/sh
  > if [ "$1" = --version ]; then
  >   if [ "${OCAMLRUNPARAM-}" = Xhelp ]; then
  >     echo probe >> "$PROBE_LOG"
  >     case "${MELC_GC-yes}" in
  >       yes|fail)
  >         echo 'Known GC tweaks:' >&2
  >         echo 'small_heap_limit (initial value 262144)' >&2 ;;
  >     esac
  >     if [ "${MELC_GC-yes}" = fail ]; then exit 1; fi
  >   fi
  >   echo 'Melange test (Using OCaml:99.0.0+Melange)'
  >   exit 0
  > fi
  > if [ "$1" != --where ]; then
  >   previous=
  >   for arg do
  >     if [ "$previous" = -o ]; then
  >       case "$arg" in
  >         *.cmj) phase=compile ;;
  >         *.cmi) phase=interface ;;
  >         *.js) phase=emit ;;
  >       esac
  >     fi
  >     previous=$arg
  >   done
  >   printf '%s ocaml=%s legacy=%s man=%s runtime=%s\n' "$phase" \
  >     "${OCAMLRUNPARAM-unset}" "${CAMLRUNPARAM-unset}" \
  >     "$MANPATH" "${MELC_GC-yes}"
  > fi
  > unset OCAMLRUNPARAM CAMLRUNPARAM
  > exec "$REAL_MELC" "$@"
  > EOF
  $ cat > bin/ocaml-wrapper <<'EOF'
  > #!/bin/sh
  > if [ "$1" = -config ] && [ -n "${REPORTED_OCAML_VERSION-}" ]; then
  >   "$REAL_OCAML_BIN/${0##*/}" "$@" |
  >     sed "s/^version:.*/version: $REPORTED_OCAML_VERSION/"
  > else
  >   exec "$REAL_OCAML_BIN/${0##*/}" "$@"
  > fi
  > EOF
  $ chmod +x bin/melc bin/ocaml-wrapper
  $ for tool in "$REAL_OCAML_BIN"/ocaml*; do
  >   name=${tool##*/}
  >   case "$name" in
  >     ocamlc|ocamlc.opt) ln -s ocaml-wrapper "bin/$name" ;;
  >     *) ln -s "$tool" "bin/$name" ;;
  >   esac
  > done
  $ export PATH="$PWD/bin:$PATH"
  $ cat > dune <<'EOF'
  > (library
  >  (name lib)
  >  (modules lib)
  >  (wrapped false)
  >  (modes melange))
  > (melange.emit
  >  (target output)
  >  (modules main)
  >  (libraries lib)
  >  (emit_stdlib false))
  > (rule
  >  (alias runtime-env)
  >  (action
  >   (system
  >    "printf 'action: %s %s\n' \\
  >      \"${OCAMLRUNPARAM-unset}\" \"${CAMLRUNPARAM-unset}\"")))
  > EOF
  $ cp dune dune.base
  $ echo 'val message : string' > lib.mli
  $ echo 'let message = "hello"' > lib.ml
  $ echo 'let message = Lib.message' > main.ml
  $ build () {
  >   : > "$PROBE_LOG"
  >   if dune build --display quiet --cache=disabled \
  >     --build-dir "_build_$1" @all > gc.log 2>&1; then
  >     sort -u gc.log
  >   else
  >     cat gc.log
  >     return 1
  >   fi
  > }
  $ build defaults
  compile ocaml=unset legacy=unset man=parent runtime=yes
  emit ocaml=unset legacy=unset man=parent runtime=yes
  interface ocaml=unset legacy=unset man=parent runtime=yes
  $ wc -l < "$PROBE_LOG" | tr -d ' '
  0
  $ dune build --build-dir _build_defaults @runtime-env
  action: unset unset

Explicit parameters, including empty values, are preserved without probing.

  $ OCAMLRUNPARAM=s=123456 build explicit
  compile ocaml=s=123456 legacy=unset man=parent runtime=yes
  emit ocaml=s=123456 legacy=unset man=parent runtime=yes
  interface ocaml=s=123456 legacy=unset man=parent runtime=yes
  $ CAMLRUNPARAM=s=123456 build legacy
  compile ocaml=unset legacy=s=123456 man=parent runtime=yes
  emit ocaml=unset legacy=s=123456 man=parent runtime=yes
  interface ocaml=unset legacy=s=123456 man=parent runtime=yes
  $ OCAMLRUNPARAM= build empty
  compile ocaml= legacy=unset man=parent runtime=yes
  emit ocaml= legacy=unset man=parent runtime=yes
  interface ocaml= legacy=unset man=parent runtime=yes
  $ CAMLRUNPARAM= build legacy-empty
  compile ocaml=unset legacy= man=parent runtime=yes
  emit ocaml=unset legacy= man=parent runtime=yes
  interface ocaml=unset legacy= man=parent runtime=yes
  $ wc -l < "$PROBE_LOG" | tr -d ' '
  0

Missing support or a failed probe must not prevent compilation, regardless of
the OCaml version reported by Melange or the context compiler.

  $ MELC_GC=no build unsupported
  compile ocaml=unset legacy=unset man=parent runtime=no
  emit ocaml=unset legacy=unset man=parent runtime=no
  interface ocaml=unset legacy=unset man=parent runtime=no
  $ MELC_GC=fail build failed-probe
  compile ocaml=unset legacy=unset man=parent runtime=fail
  emit ocaml=unset legacy=unset man=parent runtime=fail
  interface ocaml=unset legacy=unset man=parent runtime=fail
  $ REPORTED_OCAML_VERSION=4.14.0 build older-context
  compile ocaml=unset legacy=unset man=parent runtime=yes
  emit ocaml=unset legacy=unset man=parent runtime=yes
  interface ocaml=unset legacy=unset man=parent runtime=yes

The action environment is extended once. Directory-specific runtime parameters
also suppress defaults, including an empty legacy setting.

  $ cat >> dune <<'EOF'
  > (env (_ (env-vars (MANPATH local))))
  > EOF
  $ build path
  compile ocaml=unset legacy=unset man=local runtime=yes
  emit ocaml=unset legacy=unset man=local runtime=yes
  interface ocaml=unset legacy=unset man=local runtime=yes
  $ cp dune.base dune
  $ cat >> dune <<'EOF'
  > (env (_ (env-vars (OCAMLRUNPARAM s=456789))))
  > EOF
  $ build directory
  compile ocaml=s=456789 legacy=unset man=parent runtime=yes
  emit ocaml=s=456789 legacy=unset man=parent runtime=yes
  interface ocaml=s=456789 legacy=unset man=parent runtime=yes
  $ cp dune.base dune
  $ cat >> dune <<'EOF'
  > (env (_ (env-vars (CAMLRUNPARAM ""))))
  > EOF
  $ build directory-empty
  compile ocaml=unset legacy= man=parent runtime=yes
  emit ocaml=unset legacy= man=parent runtime=yes
  interface ocaml=unset legacy= man=parent runtime=yes

The compiler wrapper can select different runtimes in different directories.

  $ cp dune.base dune
  $ mkdir unsupported
  $ cat > unsupported/dune <<'EOF'
  > (env (_ (env-vars (MELC_GC no))))
  > (library
  >  (name unsupported)
  >  (wrapped false)
  >  (modes melange))
  > EOF
  $ echo 'let message = "hello"' > unsupported/unsupported.ml
  $ build environments
  compile ocaml=unset legacy=unset man=parent runtime=no
  compile ocaml=unset legacy=unset man=parent runtime=yes
  emit ocaml=unset legacy=unset man=parent runtime=yes
  interface ocaml=unset legacy=unset man=parent runtime=yes
