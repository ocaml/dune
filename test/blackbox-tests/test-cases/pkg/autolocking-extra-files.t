Test to make sure that autlocking with extra files works

  $ mkrepo
  $ add_mock_repo_if_needed
  $ mkpkg ocamlbuild <<EOF
  > build: [
  >  ["cat" "foo"]
  > ]
  > patches: ["fix.patch"]
  > extra-files: [
  >  "fix.patch"
  > ]
  > EOF

  $ mkdir $mock_packages/ocamlbuild/ocamlbuild.0.0.1/files
  $ cat > $mock_packages/ocamlbuild/ocamlbuild.0.0.1/files/fix.patch <<EOF
  > diff --git a/foo b/foo
  > new file mode 100644
  > index 0000000..1111111
  > --- /dev/null
  > +++ b/foo
  > @@ -0,0 +1 @@
  > +patched
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > 
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends ocamlbuild))
  > EOF

  $ enable_pkg
  $ dune pkg enabled && echo "package management enabled"
  package management enabled

Autolocking correctly handles packages with extra files (patches)

  $ dune build @pkg-install 2>&1
  patched

