Dune should attach synopsis to alias, but should not attach deps synopsis.

  $ cat > dune-project << EOF
  > (lang dune 3.19)
  > EOF
  $ cat > dune << EOF
  > (rule
  >  (target dep-file)
  >  (alias dep-rule)
  >  (synopsis "Synopsis for rule used as deps")
  >  (action
  >   (run touch %{target})))
  > (rule
  >  (deps dep-synopsis)
  >  (alias has-dep-rule)
  >  (synopsis "Synopsis for rule that has deps")
  >  (action
  >   (run echo "Text from dep" > dep-file)))
  > EOF

  $ dune show targets
  dep-file
    - dune:1 Synopsis for rule used as deps
  dune
  dune-project
  $ dune show aliases
  all
  default
    - This alias corresponds to the default argument for dune build. More
      information on
      https://dune.readthedocs.io/en/latest/reference/aliases/default.html
  dep-rule
    - dune:1 Synopsis for rule used as deps
  fmt
  has-dep-rule
    - dune:7 Synopsis for rule that has deps
  ocaml-index
  pkg-install
