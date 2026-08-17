A package dependency currently omits a required library in a nested namespace.
Once the library is materialized, metadata from structural namespace nodes must
not leak into the filtered META file.

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
  File "dune", lines 1-8, characters 0-223:
  1 | (rule
  2 |  (target marker)
  3 |  (deps (package ns-root))
  4 |  (action
  5 |   (with-stdout-to %{target}
  6 |    (run %{bin:ocamlfind} query
  7 |     -format "%(intermediate_marker)|%(selected_marker)|%d/selected.cmi"
  8 |     ns-support.middle.selected))))
  ocamlfind: Package `ns-support.middle.selected' not found
  [1]
