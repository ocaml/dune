A package dependency includes required libraries in nested namespaces without
leaking metadata from structural namespace nodes into the filtered META file.

  $ make_dune_project 3.24
  $ cat >>dune-project <<EOF
  > (package (name ns-root))
  > (package (name ns-support))
  > EOF

  $ mkdir root support
  $ cat >root/dune <<'EOF'
  > (library
  >  (name root)
  >  (public_name ns-root)
  >  (libraries ns-support.middle.selected))
  > EOF
  $ echo 'let value = ()' >root/root.ml

  $ cat >support/dune <<'EOF'
  > (library
  >  (name selected)
  >  (public_name ns-support.middle.selected))
  > EOF
  $ echo 'let value = ()' >support/selected.ml

  $ cat >META.ns-support.template <<'EOF'
  > package "middle" (
  >  directory = "middle"
  >  intermediate_marker = "drop"
  >  package "selected" (
  >   directory = "selected"
  >   selected_marker = "keep"
  >  )
  > )
  > EOF

  $ cat >dune <<'EOF'
  > (rule
  >  (target marker)
  >  (deps (package ns-root))
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{bin:ocamlfind} query
  >     -format "%(intermediate_marker)|%(selected_marker)|%d/selected.cmi"
  >     ns-support.middle.selected))))
  > EOF

  $ dune build marker
  $ cat _build/default/marker | censor
  |keep|$PWD/_build/install/default/.packages/$DIGEST/lib/ns-support/middle/selected/selected.cmi
  $ test -f "$(cut -d'|' -f3 _build/default/marker)"
