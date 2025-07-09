Dune should show all synopses with their location for all rules to which alias attached.

  $ cat > dune-project << EOF
  > (lang dune 3.19)
  > EOF
  $ cat > dune << EOF
  > (rule
  >  (target file1.ml)
  >  (alias rule-with-synopses)
  >  (synopsis "Rule creates file1.ml")
  >  (action
  >    (write-file %{target} "")))
  > 
  > (rule
  >  (target file2.ml)
  >  (alias rule-with-synopses)
  >  (synopsis "Rule creates file2.ml")
  >  (action
  >    (write-file %{target} "")))
  > EOF

  $ dune show targets
  dune
  dune-project
  file1.ml
    - dune:1 Rule creates file1.ml
  file2.ml
    - dune:8 Rule creates file2.ml
  $ dune show aliases
  all
  default
  fmt
  ocaml-index
  pkg-install
  rule-with-synopses
    - dune:1 Rule creates file1.ml
    - dune:8 Rule creates file2.ml
