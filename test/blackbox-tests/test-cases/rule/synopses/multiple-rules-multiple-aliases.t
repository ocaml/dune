Dune should show all synopses with their location for all rules to aliases attached.

  $ cat > dune-project << EOF
  > (lang dune 3.19)
  > EOF
  $ cat > dune << EOF
  > (rule
  >  (targets file1.ml)
  >  (aliases rule-with-synopses and-another-rule)
  >  (synopsis "Rule creates file1.ml")
  >  (action
  >    (write-file file1.ml "")))
  > 
  > (rule
  >  (targets file2.ml)
  >  (aliases rule-with-synopses and-another-rule)
  >  (synopsis "Rule creates file2.ml")
  >  (action
  >    (write-file file2.ml "")))
  > 
  > (rule
  >  (targets file3.ml)
  >  (alias and-another-rule)
  >  (synopsis "Rule creates file3.ml")
  >  (action
  >    (write-file file3.ml "")))
  > EOF

  $ dune show targets
  dune
  dune-project
  file1.ml
    - dune:1 Rule creates file1.ml
  file2.ml
    - dune:8 Rule creates file2.ml
  file3.ml
    - dune:15 Rule creates file3.ml
  $ dune show aliases
  all
  and-another-rule
    - dune:1 Rule creates file1.ml
    - dune:8 Rule creates file2.ml
    - dune:15 Rule creates file3.ml
  default
    - This alias corresponds to the default argument for dune build. More
      information on
      https://dune.readthedocs.io/en/latest/reference/aliases/default.html
  fmt
  ocaml-index
  pkg-install
  rule-with-synopses
    - dune:1 Rule creates file1.ml
    - dune:8 Rule creates file2.ml
