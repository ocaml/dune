Test that %{pkg:...} introduces an implicit dependency on the package and
makes its artifacts accessible.

  $ cat >dune-project <<EOF
  > (lang dune 3.23)
  > (expand_aliases_in_sandbox)
  > (package (name foo))
  > EOF

  $ mkdir foo

  $ cat >foo/dune <<EOF
  > (install
  >  (section share)
  >  (package foo)
  >  (files (data.txt as data.txt)))
  > EOF

  $ cat >foo/data.txt <<EOF
  > some data
  > EOF

Verify via dune rules --deps that the install alias is registered without
any explicit (deps (package foo)):

  $ cat >dune <<EOF
  > (rule
  >  (target dep-output)
  >  (action (with-stdout-to %{target} (echo %{pkg:foo:share}))))
  > EOF

  $ dune rules --deps _build/default/dep-output 2>&1 | grep -i alias
  ((Alias ((dir (In_build_dir _build/default)) (name .foo-files))))

With (expand_aliases_in_sandbox) enabled, the implicit dependency is
sufficient to make artifacts accessible at the expanded path:

  $ cat >dune <<EOF
  > (rule
  >  (alias test-read-artifact)
  >  (action (system "cat %{pkg:foo:share}/data.txt")))
  > EOF

  $ dune build @test-read-artifact 2>&1
  some data
