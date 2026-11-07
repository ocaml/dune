Compilation, archiving, linking and dependency analysis preserve the directory
environment, as do user actions and executables.

  $ make_dune_project 3.25
  $ unset OCAMLRUNPARAM CAMLRUNPARAM
  $ export REAL_OCAML_BIN=$(dirname "$(command -v ocamlc)")
  $ export GC_LOG="$PWD/gc.log"
  $ mkdir bin
  $ cat > bin/wrapper <<'EOF'
  > #!/bin/sh
  > if [ "$1" = -config ]; then
  >   if [ -n "${REPORTED_OCAML_VERSION-}" ]; then
  >     "$REAL_OCAML_BIN/${0##*/}" "$@" |
  >       sed "s/^version:.*/version: $REPORTED_OCAML_VERSION/"
  >   else
  >     exec "$REAL_OCAML_BIN/${0##*/}" "$@"
  >   fi
  > else
  >   printf '%s OCAMLRUNPARAM=%s CAMLRUNPARAM=%s\n' "${0##*/}" \
  >     "${OCAMLRUNPARAM-unset}" "${CAMLRUNPARAM-unset}" >> "$GC_LOG"
  >   exec "$REAL_OCAML_BIN/${0##*/}" "$@"
  > fi
  > EOF
  $ chmod +x bin/wrapper
  $ for tool in "$REAL_OCAML_BIN"/ocaml*; do
  >   name=${tool##*/}
  >   case "$name" in
  >     ocamlc|ocamlc.opt|ocamlopt|ocamlopt.opt|ocamldep|ocamldep.opt)
  >       ln -s wrapper "bin/$name" ;;
  >     *) ln -s "$tool" "bin/$name" ;;
  >   esac
  > done
  $ export PATH="$PWD/bin:$PATH"
  $ cat > dune <<'EOF'
  > (library (name lib) (modules lib helper))
  > (executable (name main) (modules main) (libraries lib))
  > (rule
  >  (alias runtest)
  >  (action
  >   (run sh -c
  >    "printf 'action: %s %s\n' \\
  >      \"${OCAMLRUNPARAM-unset}\" \"${CAMLRUNPARAM-unset}\"")))
  > EOF
  $ echo 'let message = "hello"' > helper.ml
  $ echo 'let message = Helper.message' > lib.ml
  $ cat > main.ml <<'EOF'
  > let () =
  >   let get name =
  >     match Sys.getenv_opt name with None -> "unset" | Some value -> value
  >   in
  >   Printf.printf "%s: %s %s\n" Lib.message
  >     (get "OCAMLRUNPARAM") (get "CAMLRUNPARAM")
  > EOF
  $ build () {
  >   : > "$GC_LOG"
  >   dune build --cache=disabled --build-dir "_build_$1" @all
  >   sort -u "$GC_LOG"
  > }
  $ build defaults
  ocamlc.opt OCAMLRUNPARAM=unset CAMLRUNPARAM=unset
  ocamldep.opt OCAMLRUNPARAM=unset CAMLRUNPARAM=unset
  ocamlopt.opt OCAMLRUNPARAM=unset CAMLRUNPARAM=unset
  $ dune build --build-dir _build_defaults @runtest
  action: unset unset
  $ dune exec --build-dir _build_defaults ./main.exe
  hello: unset unset

Explicit runtime parameters, including an empty value, are preserved.

  $ OCAMLRUNPARAM=s=123456 build explicit
  ocamlc.opt OCAMLRUNPARAM=s=123456 CAMLRUNPARAM=unset
  ocamldep.opt OCAMLRUNPARAM=s=123456 CAMLRUNPARAM=unset
  ocamlopt.opt OCAMLRUNPARAM=s=123456 CAMLRUNPARAM=unset
  $ CAMLRUNPARAM=s=123456 build legacy
  ocamlc.opt OCAMLRUNPARAM=unset CAMLRUNPARAM=s=123456
  ocamldep.opt OCAMLRUNPARAM=unset CAMLRUNPARAM=s=123456
  ocamlopt.opt OCAMLRUNPARAM=unset CAMLRUNPARAM=s=123456
  $ OCAMLRUNPARAM= build empty
  ocamlc.opt OCAMLRUNPARAM= CAMLRUNPARAM=unset
  ocamldep.opt OCAMLRUNPARAM= CAMLRUNPARAM=unset
  ocamlopt.opt OCAMLRUNPARAM= CAMLRUNPARAM=unset

Runtime parameters are unchanged for older compilers as well.

  $ REPORTED_OCAML_VERSION=5.4.0 build older
  ocamlc.opt OCAMLRUNPARAM=unset CAMLRUNPARAM=unset
  ocamldep.opt OCAMLRUNPARAM=unset CAMLRUNPARAM=unset
  ocamlopt.opt OCAMLRUNPARAM=unset CAMLRUNPARAM=unset

Directory-specific environment settings take precedence as well.

  $ cat >> dune <<'EOF'
  > (env
  >  (_ (env-vars (OCAMLRUNPARAM Xsmall_heap_limit=4194304))))
  > EOF
  $ build stanza
  ocamlc.opt OCAMLRUNPARAM=Xsmall_heap_limit=4194304 CAMLRUNPARAM=unset
  ocamldep.opt OCAMLRUNPARAM=Xsmall_heap_limit=4194304 CAMLRUNPARAM=unset
  ocamlopt.opt OCAMLRUNPARAM=Xsmall_heap_limit=4194304 CAMLRUNPARAM=unset
