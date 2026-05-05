%{pkg:...} is not allowed inside (install) stanzas since they use no-deps
pform expansion.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name foo))
  > (package (name bar))
  > EOF

  $ mkdir foo bar

  $ cat >foo/dune <<EOF
  > (install
  >  (section share)
  >  (package foo)
  >  (files (src.txt as dest.txt)))
  > EOF

  $ cat >foo/src.txt <<EOF
  > foo data
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (target generated.txt)
  >  (action (with-stdout-to %{target} (echo "generated"))))
  > (install
  >  (section share)
  >  (package bar)
  >  (files (generated.txt as %{pkg:foo:share:dest.txt}/nested.txt)))
  > EOF

  $ dune build @install 2>&1
  File "dune", line 7, characters 26-51:
  7 |  (files (generated.txt as %{pkg:foo:share:dest.txt}/nested.txt)))
                                ^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: %{pkg:..} isn't allowed in this position.
  [1]
