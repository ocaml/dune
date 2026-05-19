%{bin:...} for a system binary found via PATH.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > EOF
  $ cat >dune <<'EOF'
  > (rule
  >  (with-stdout-to bin-path
  >   (echo %{bin:sh})))
  > EOF

The pform resolves to the same path as which:

  $ dune build bin-path
  $ cat _build/default/bin-path | dune_cmd subst "$(which sh)" '$WHICH_SH'
  $WHICH_SH

The rule depends on the system binary as an external path:

  $ dune rules --format=json _build/default/bin-path \
  >   | jq 'include "dune"; .[] | ruleDepFilePaths' \
  >   | dune_cmd subst "$(which sh)" '$WHICH_SH'
  "$WHICH_SH"
