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

Reordering two includes changes which copy of a file is visible. The
files themselves and the action stay the same.

  $ echo '((package addon))' > addon.sexp
  $ echo '((package leaf))' > leaf.sexp
  $ cat >> dune <<'EOF'
  > (install
  >  (package addon)
  >  (section (site (foo data)))
  >  (files (addon.txt as choice.txt)))
  > (install
  >  (package leaf)
  >  (section (site (foo data)))
  >  (files (leaf.txt as choice.txt)))
  > (rule
  >  (target chosen)
  >  (deps (include addon.sexp) (include leaf.sexp))
  >  (action
  >   (with-stdout-to %{target}
  >    (with-accepted-exit-codes (or 0 1) (run ./main.exe choice.txt)))))
  > EOF
  $ dune build chosen
  $ cat _build/default/chosen
  choice.txt: missing
  $ dune_cmd subst 'addon.sexp\) \(include leaf.sexp' \
  >   'leaf.sexp) (include addon.sexp' < dune > reordered
  $ mv reordered dune
  $ dune build chosen
  $ cat _build/default/chosen
  choice.txt: missing
