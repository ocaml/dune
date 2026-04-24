Test interaction between scope stanzas, vendored packages, and -p flag.

The -p flag includes all vendored packages automatically. Scope stanzas
control which packages are visible. The interaction should be:
- Dune_load.mask: includes ALL vendored packages (approximation)
- Scope.DB.mask: includes only scope-visible vendored packages (refined)

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > (package (name mypkg))
  > EOF

Create two vendored directories with the same package name:

  $ mkdir -p vendor_a vendor_b

  $ cat > vendor_a/dune-project << EOF
  > (lang dune 3.22)
  > (package (name bar))
  > EOF

  $ cat > vendor_a/dune << EOF
  > (library
  >  (name bar_a)
  >  (public_name bar))
  > EOF

  $ cat > vendor_a/bar_a.ml << EOF
  > let msg = "bar from vendor_a"
  > EOF

  $ cat > vendor_b/dune-project << EOF
  > (lang dune 3.22)
  > (package (name bar))
  > EOF

  $ cat > vendor_b/dune << EOF
  > (library
  >  (name bar_b)
  >  (public_name bar))
  > EOF

  $ cat > vendor_b/bar_b.ml << EOF
  > let msg = "bar from vendor_b"
  > EOF

Mark both as vendored, but only expose vendor_a/bar via scope:

  $ cat > dune << EOF
  > (vendored_dirs vendor_a vendor_b)
  > (scope
  >  (dir vendor_a)
  >  (packages bar))
  > (scope
  >  (dir vendor_b)
  >  (packages))
  > (executable
  >  (name main)
  >  (public_name mypkg)
  >  (libraries bar))
  > EOF

  $ cat > main.ml << EOF
  > let () = print_endline Bar_a.msg
  > EOF

Build without -p should work (scope makes vendor_a/bar visible):

  $ dune exec ./main.exe
  bar from vendor_a

Build with -p mypkg should also work (vendored package included):

  $ dune exec -p mypkg ./main.exe
  bar from vendor_a

Now test with a non-vendored local package that shadows the vendored ones:

  $ mkdir -p local_bar

  $ cat > local_bar/dune-project << EOF
  > (lang dune 3.22)
  > (package (name bar))
  > EOF

  $ cat > local_bar/dune << EOF
  > (library
  >  (name bar_local)
  >  (public_name bar))
  > EOF

  $ cat > local_bar/bar_local.ml << EOF
  > let msg = "bar from local"
  > EOF

Update scope to expose local_bar instead of vendor_a (hide both vendored):

  $ cat > dune << EOF
  > (vendored_dirs vendor_a vendor_b)
  > (scope
  >  (dir vendor_a)
  >  (packages))
  > (scope
  >  (dir vendor_b)
  >  (packages))
  > (scope
  >  (dir local_bar)
  >  (packages bar))
  > (executable
  >  (name main)
  >  (public_name mypkg)
  >  (libraries bar))
  > EOF

  $ cat > main.ml << EOF
  > let () = print_endline Bar_local.msg
  > EOF

Build without -p should work:

  $ dune exec ./main.exe
  bar from local

Build with -p mypkg - vendored packages are always included automatically.
Even though scope hides vendor_a/bar and vendor_b/bar, the name "bar" is
still in the vendored set (approximation), so local_bar/bar is visible:

  $ dune exec -p mypkg ./main.exe
  bar from local

Explicitly including bar in -p gives an error because the source tree
traversal (before scope filtering) sees vendored packages named "bar":

  $ dune exec -p mypkg,bar ./main.exe
  Error: Package bar is vendored and so will never be masked. It is redundant
  to pass it to --only-packages.
  [1]
