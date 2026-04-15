Test that %{pkg:foo:...} works from a rule inside the package directory itself.
This verifies the alias dir computation is correct when the referencing rule
lives in the same directory as the package.

  $ cat >dune-project <<EOF
  > (lang dune 3.23)
  > (expand_aliases_in_sandbox)
  > (package (name foo) (dir foo))
  > EOF

  $ mkdir foo

  $ cat >foo/dune <<EOF
  > (executable
  >  (name foo_bin)
  >  (public_name foo-bin)
  >  (package foo))
  > (install
  >  (section share)
  >  (package foo)
  >  (files (data.txt as data.txt)))
  > (rule
  >  (alias test-self-ref)
  >  (action
  >   (progn
  >    (system "echo share: %{pkg:foo:share}")
  >    (system "cat %{pkg:foo:share}/data.txt"))))
  > EOF

  $ cat >foo/foo_bin.ml <<EOF
  > let () = print_endline "I am foo"
  > EOF

  $ cat >foo/data.txt <<EOF
  > some data
  > EOF

  $ dune build @test-self-ref 2>&1
  share: ../../install/default/share/foo
  some data
