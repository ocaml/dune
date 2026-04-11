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

The build succeeds — %{lib:...} falls back to the package install path:

  $ dune build out.txt

  $ cat _build/default/out.txt
  ../install/default/lib/foo/data.txt
