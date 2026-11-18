A cram test should see sites from both matching stanzas, including direct
dependencies and dependencies in nested includes.

  $ write_package_site_test_project
  $ cat >> dune-project <<'EOF'
  > (package (name addon))
  > (package (name leaf))
  > EOF
  $ echo addon > addon.txt
  $ echo leaf > leaf.txt
  $ cat >> dune <<'EOF'
  > (install (package addon) (section (site (foo data))) (files addon.txt))
  > (install (package leaf) (section (site (foo data))) (files leaf.txt))
  > (cram (applies_to :whole_subtree) (deps (package foo)))
  > EOF

  $ mkdir checks
  $ echo '((include nested.sexp))' > checks/deps.sexp
  $ echo '((package addon))' > checks/nested.sexp
  $ echo '(cram (deps (package leaf) (include deps.sexp)))' > checks/dune
  $ cat > checks/sites.t <<'EOF'
  > Read files from all three packages.
  >   $ main foo.txt addon.txt leaf.txt
  >   foo.txt: missing
  >   addon.txt: missing
  >   leaf.txt: missing
  >   [1]
  > EOF
  $ dune runtest
