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
  >   if [ -n "${REPORTED_OCAML_WORD_SIZE-}" ]; then
  >     "$REAL_OCAML_BIN/${0##*/}" "$@" |
  >       sed "s/^word_size:.*/word_size: $REPORTED_OCAML_WORD_SIZE/"
  >   elif [ -n "${REPORTED_OCAML_VERSION-}" ]; then
  >     "$REAL_OCAML_BIN/${0##*/}" "$@" |
  >       sed "s/^version:.*/version: $REPORTED_OCAML_VERSION/"
  >   else
  >     exec "$REAL_OCAML_BIN/${0##*/}" "$@"
  >   fi
  > else
  >   if [ "${REPORT_COMPILER_ENV-}" = yes ]; then
  >     printf '%s MANPATH=%s\n' "${0##*/}" "$MANPATH" >> "$GC_LOG"
  >   fi
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
  >   build_dir="_build_$1"
  >   shift
  >   dune build --cache=disabled --build-dir "$build_dir" "$@" @all
  >   sort -u "$GC_LOG"
  > }
  $ build defaults
  ocamlc.opt OCAMLRUNPARAM=s=2M,Xsmall_heap_limit=16777216 CAMLRUNPARAM=unset
  ocamldep.opt OCAMLRUNPARAM=s=2M,Xsmall_heap_limit=16777216 CAMLRUNPARAM=unset
  ocamlopt.opt OCAMLRUNPARAM=s=2M,Xsmall_heap_limit=16777216 CAMLRUNPARAM=unset
  $ dune build --build-dir _build_defaults @runtest
  action: unset unset
  $ dune exec --build-dir _build_defaults ./main.exe
  hello: unset unset

Compilers and dependency analysis share the same GC defaults, independent of
build profile. Unrelated actions and programs are unchanged.

  $ build custom --profile custom
  ocamlc.opt OCAMLRUNPARAM=s=2M,Xsmall_heap_limit=16777216 CAMLRUNPARAM=unset
  ocamldep.opt OCAMLRUNPARAM=s=2M,Xsmall_heap_limit=16777216 CAMLRUNPARAM=unset
  ocamlopt.opt OCAMLRUNPARAM=s=2M,Xsmall_heap_limit=16777216 CAMLRUNPARAM=unset

  $ build release --profile release
  ocamlc.opt OCAMLRUNPARAM=s=2M,Xsmall_heap_limit=16777216 CAMLRUNPARAM=unset
  ocamldep.opt OCAMLRUNPARAM=s=2M,Xsmall_heap_limit=16777216 CAMLRUNPARAM=unset
  ocamlopt.opt OCAMLRUNPARAM=s=2M,Xsmall_heap_limit=16777216 CAMLRUNPARAM=unset
  $ dune build --profile release --build-dir _build_release @runtest
  action: unset unset
  $ dune exec --profile release --build-dir _build_release ./main.exe
  hello: unset unset
  $ REPORTED_OCAML_VERSION=5.4.0 build older_release --profile release
  ocamlc.opt OCAMLRUNPARAM=s=2M CAMLRUNPARAM=unset
  ocamldep.opt OCAMLRUNPARAM=s=2M CAMLRUNPARAM=unset
  ocamlopt.opt OCAMLRUNPARAM=s=2M CAMLRUNPARAM=unset

Use the context compiler's word size for the nominal byte-to-GC-work conversion.

  $ REPORTED_OCAML_WORD_SIZE=32 build narrow_words
  ocamlc.opt OCAMLRUNPARAM=s=2M,Xsmall_heap_limit=33554432 CAMLRUNPARAM=unset
  ocamldep.opt OCAMLRUNPARAM=s=2M,Xsmall_heap_limit=33554432 CAMLRUNPARAM=unset
  ocamlopt.opt OCAMLRUNPARAM=s=2M,Xsmall_heap_limit=33554432 CAMLRUNPARAM=unset

Explicit OCAMLRUNPARAM settings, including an empty value, take precedence.
The legacy CAMLRUNPARAM variable does not suppress these defaults.

  $ OCAMLRUNPARAM=s=123456 build explicit --profile release
  ocamlc.opt OCAMLRUNPARAM=s=123456 CAMLRUNPARAM=unset
  ocamldep.opt OCAMLRUNPARAM=s=123456 CAMLRUNPARAM=unset
  ocamlopt.opt OCAMLRUNPARAM=s=123456 CAMLRUNPARAM=unset
  $ CAMLRUNPARAM=s=123456 build legacy --profile release
  ocamlc.opt OCAMLRUNPARAM=s=2M,Xsmall_heap_limit=16777216 CAMLRUNPARAM=s=123456
  ocamldep.opt OCAMLRUNPARAM=s=2M,Xsmall_heap_limit=16777216 CAMLRUNPARAM=s=123456
  ocamlopt.opt OCAMLRUNPARAM=s=2M,Xsmall_heap_limit=16777216 CAMLRUNPARAM=s=123456
  $ OCAMLRUNPARAM= build empty --profile release
  ocamlc.opt OCAMLRUNPARAM= CAMLRUNPARAM=unset
  ocamldep.opt OCAMLRUNPARAM= CAMLRUNPARAM=unset
  ocamlopt.opt OCAMLRUNPARAM= CAMLRUNPARAM=unset

  $ CAMLRUNPARAM= build legacy_empty --profile release
  ocamlc.opt OCAMLRUNPARAM=s=2M,Xsmall_heap_limit=16777216 CAMLRUNPARAM=
  ocamldep.opt OCAMLRUNPARAM=s=2M,Xsmall_heap_limit=16777216 CAMLRUNPARAM=
  ocamlopt.opt OCAMLRUNPARAM=s=2M,Xsmall_heap_limit=16777216 CAMLRUNPARAM=

Older compilers get the larger minor heap, but not the unsupported major-GC
setting.

  $ REPORTED_OCAML_VERSION=5.4.0 build older
  ocamlc.opt OCAMLRUNPARAM=s=2M CAMLRUNPARAM=unset
  ocamldep.opt OCAMLRUNPARAM=s=2M CAMLRUNPARAM=unset
  ocamlopt.opt OCAMLRUNPARAM=s=2M CAMLRUNPARAM=unset

Directory-specific environment settings take precedence as well.

  $ cat >> dune <<'EOF'
  > (env
  >  (_
  >   (env-vars
  >    (OCAMLRUNPARAM Xsmall_heap_limit=4194304)
  >    (MANPATH /compiler-env-manpath))))
  > EOF
  $ build stanza --profile release
  ocamlc.opt OCAMLRUNPARAM=Xsmall_heap_limit=4194304 CAMLRUNPARAM=unset
  ocamldep.opt OCAMLRUNPARAM=Xsmall_heap_limit=4194304 CAMLRUNPARAM=unset
  ocamlopt.opt OCAMLRUNPARAM=Xsmall_heap_limit=4194304 CAMLRUNPARAM=unset

Print a path-like environment variable inside each compiler tool. Directory
settings must not gain duplicate entries when constructing the action
environment.

  $ REPORT_COMPILER_ENV=yes build paths | grep ' MANPATH='
  ocamlc.opt MANPATH=/compiler-env-manpath
  ocamldep.opt MANPATH=/compiler-env-manpath
  ocamlopt.opt MANPATH=/compiler-env-manpath

An empty directory-level OCAMLRUNPARAM is preserved.

  $ sed -i.bak \
  >   's/OCAMLRUNPARAM Xsmall_heap_limit=4194304/OCAMLRUNPARAM ""/' dune
  $ build stanza_empty
  ocamlc.opt OCAMLRUNPARAM= CAMLRUNPARAM=unset
  ocamldep.opt OCAMLRUNPARAM= CAMLRUNPARAM=unset
  ocamlopt.opt OCAMLRUNPARAM= CAMLRUNPARAM=unset
