Dune's internal runtime defaults are not passed to actions:

  $ make_dune_project 3.23
  $ cat >dune <<'EOF'
  > (rule
  >  (alias runtest)
  >  (action
  >   (bash
  >    "printf 'OCAMLRUNPARAM=%s CAMLRUNPARAM=%s MARKER=%s\\n' \\
  >      \"${OCAMLRUNPARAM-unset}\" \\
  >      \"${CAMLRUNPARAM-unset}\" \\
  >      \"${DUNE_INTERNAL__RUNTIME_DEFAULTS-unset}\"")))
  > EOF

  $ env -u OCAMLRUNPARAM -u CAMLRUNPARAM dune build @runtest
  OCAMLRUNPARAM=unset CAMLRUNPARAM=unset MARKER=unset

Explicit modern and legacy runtime parameters are preserved:

  $ OCAMLRUNPARAM=s=123456 dune exec -- sh -c \
  > 'printf "OCAMLRUNPARAM=%s CAMLRUNPARAM=%s MARKER=%s\n" \
  >   "${OCAMLRUNPARAM-unset}" \
  >   "${CAMLRUNPARAM-unset}" \
  >   "${DUNE_INTERNAL__RUNTIME_DEFAULTS-unset}"'
  OCAMLRUNPARAM=s=123456 CAMLRUNPARAM=unset MARKER=unset
  $ CAMLRUNPARAM=s=123456 dune exec -- sh -c \
  > 'printf "OCAMLRUNPARAM=%s CAMLRUNPARAM=%s MARKER=%s\n" \
  >   "${OCAMLRUNPARAM-unset}" \
  >   "${CAMLRUNPARAM-unset}" \
  >   "${DUNE_INTERNAL__RUNTIME_DEFAULTS-unset}"'
  OCAMLRUNPARAM=unset CAMLRUNPARAM=s=123456 MARKER=unset
