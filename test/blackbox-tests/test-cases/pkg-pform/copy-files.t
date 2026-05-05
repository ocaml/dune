%{pkg:...} is not allowed inside (copy_files) stanzas since they use no-deps
pform expansion.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name foo))
  > EOF

  $ mkdir foo

  $ cat >foo/dune <<EOF
  > (install
  >  (section share)
  >  (package foo)
  >  (files (src.txt as dest.txt)))
  > EOF

  $ cat >foo/src.txt <<EOF
  > some data
  > EOF

  $ cat >dune <<EOF
  > (copy_files %{pkg:foo:share:dest.txt})
  > EOF

  $ dune build 2>&1
  File "dune", line 1, characters 12-37:
  1 | (copy_files %{pkg:foo:share:dest.txt})
                  ^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: %{pkg:..} isn't allowed in this position.
  [1]
