%{pkg:...} is not allowed inside (copy_files) stanzas since they use no-deps
pform expansion.

  $ make_dune_project_with_package 3.24 foo

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
