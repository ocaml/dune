Bootstrap uses the GC settings supported by the selected compiler. Wrap the
compiler tools to report the settings without passing them to the real compiler.

  $ init_bootstrap
  $ unset OCAMLRUNPARAM CAMLRUNPARAM
  $ export REAL_OCAML_BIN=$(dirname "$(command -v ocamlc)")
  $ export GC_LOG="$PWD/gc.log"
  $ export REPORTED_VERSION=5.5.0
  $ mkdir tools
  $ cat > tools/compiler <<'EOF'
  > #!/bin/sh
  > tool="$REAL_OCAML_BIN/${0##*/}"
  > if [ "$1" = -config ]; then
  >   "$tool" "$@" | sed -e "s/^version:.*/version: $REPORTED_VERSION/" \
  >     -e 's/^word_size:.*/word_size: 64/'
  > else
  >   printf '%s OCAMLRUNPARAM=%s CAMLRUNPARAM=%s\n' "${0##*/}" \
  >     "${OCAMLRUNPARAM-unset}" "${CAMLRUNPARAM-unset}" >> "$GC_LOG"
  >   unset OCAMLRUNPARAM CAMLRUNPARAM
  >   exec "$tool" "$@"
  > fi
  > EOF
  $ chmod +x tools/compiler
  $ for tool in "$REAL_OCAML_BIN"/ocaml*; do
  >   name=${tool##*/}
  >   case "$name" in
  >     ocamlc|ocamlc.opt|ocamlopt|ocamlopt.opt|ocamldep|ocamldep.opt)
  >       ln -s compiler "tools/$name" ;;
  >     *) ln -s "$tool" "tools/$name" ;;
  >   esac
  > done
  $ export PATH="$PWD/tools:$PATH"

  $ make_module bin/root.ml
  $ create_dune '' <<'EOF'
  > open Root
  > let () = print_endline "Bootstrapped"
  > EOF
  ocamllex -ml -q -o boot/pps.ml boot/pps.mll
  ocaml -I +unix unix.cma $DUNEBOOT
  Bootstrapped

  $ bootstrap_gc () {
  >   : > "$GC_LOG"
  >   bootstrap.exe >bootstrap.log 2>&1 || { cat bootstrap.log; return 1; }
  >   sort -u "$GC_LOG"
  > }
  $ bootstrap_gc
  ocamldep.opt OCAMLRUNPARAM=s=2M,Xsmall_heap_limit=16777216 CAMLRUNPARAM=unset
  ocamlopt.opt OCAMLRUNPARAM=s=2M,Xsmall_heap_limit=16777216 CAMLRUNPARAM=unset
  $ REPORTED_VERSION=5.4.1 bootstrap_gc
  ocamldep.opt OCAMLRUNPARAM=s=2M CAMLRUNPARAM=unset
  ocamlopt.opt OCAMLRUNPARAM=s=2M CAMLRUNPARAM=unset

Explicit settings, including an empty OCAMLRUNPARAM, are preserved.

  $ OCAMLRUNPARAM= bootstrap_gc
  ocamldep.opt OCAMLRUNPARAM= CAMLRUNPARAM=unset
  ocamlopt.opt OCAMLRUNPARAM= CAMLRUNPARAM=unset
  $ CAMLRUNPARAM=s=123456 bootstrap_gc
  ocamldep.opt OCAMLRUNPARAM=unset CAMLRUNPARAM=s=123456
  ocamlopt.opt OCAMLRUNPARAM=unset CAMLRUNPARAM=s=123456
