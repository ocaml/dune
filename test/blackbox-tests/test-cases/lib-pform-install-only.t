%{lib:pkg:file} should work for packages that only have install stanzas
and no library definition (#3378).

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > (name foo)
  > (package (name foo) (allow_empty))
  > EOF

  $ cat > data.txt <<EOF
  > some data
  > EOF

  $ cat > dune <<EOF
  > (install
  >  (section lib)
  >  (files data.txt)
  >  (package foo))
  > 
  > (rule
  >  (target out.txt)
  >  (action (with-stdout-to %{target} (echo %{lib:foo:data.txt}))))
  > EOF

Currently fails because %{lib:...} only looks up library stanzas:

  $ dune build out.txt
  File "dune", line 8, characters 41-60:
  8 |  (action (with-stdout-to %{target} (echo %{lib:foo:data.txt}))))
                                               ^^^^^^^^^^^^^^^^^^^
  Error: Library "foo" not found.
  -> required by %{lib:foo:data.txt} at dune:8
  -> required by _build/default/out.txt
  [1]
