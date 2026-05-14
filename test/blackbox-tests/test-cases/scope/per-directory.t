Test that scope stanzas are applied per-directory, not globally.

This tests that different directories can have different scope rules for the
same packages, and that the visibility is correctly scoped to each directory.

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > EOF

Create two separate areas, each with its own vendor directory containing
the same two packages (pkg_x and pkg_y):

  $ mkdir -p area_a/vendor area_b/vendor

Area A's vendor:
  $ cat > area_a/vendor/dune-project << EOF
  > (lang dune 3.22)
  > (package (name pkg_x))
  > (package (name pkg_y))
  > EOF

  $ cat > area_a/vendor/dune << EOF
  > (library
  >  (name lib_x_a)
  >  (public_name pkg_x.from_a))
  > (library
  >  (name lib_y_a)
  >  (public_name pkg_y.from_a))
  > EOF

  $ cat > area_a/vendor/lib_x_a.ml << EOF
  > let msg = "lib_x from area_a"
  > EOF

  $ cat > area_a/vendor/lib_y_a.ml << EOF
  > let msg = "lib_y from area_a"
  > EOF

Area B's vendor (same package names, different content):
  $ cat > area_b/vendor/dune-project << EOF
  > (lang dune 3.22)
  > (package (name pkg_x))
  > (package (name pkg_y))
  > EOF

  $ cat > area_b/vendor/dune << EOF
  > (library
  >  (name lib_x_b)
  >  (public_name pkg_x.from_b))
  > (library
  >  (name lib_y_b)
  >  (public_name pkg_y.from_b))
  > EOF

  $ cat > area_b/vendor/lib_x_b.ml << EOF
  > let msg = "lib_x from area_b"
  > EOF

  $ cat > area_b/vendor/lib_y_b.ml << EOF
  > let msg = "lib_y from area_b"
  > EOF

=============================================================================
Set up different scope rules for each area:
- Area A exposes only pkg_x
- Area B exposes only pkg_y
=============================================================================

  $ cat > area_a/dune-project << EOF
  > (lang dune 3.22)
  > EOF

  $ cat > area_a/dune << EOF
  > (scope
  >  (dir vendor)
  >  (packages pkg_x))
  > EOF

  $ cat > area_b/dune-project << EOF
  > (lang dune 3.22)
  > EOF

  $ cat > area_b/dune << EOF
  > (scope
  >  (dir vendor)
  >  (packages pkg_y))
  > EOF

=============================================================================
Test: Executable in area_a should see pkg_x but NOT pkg_y
=============================================================================

  $ cat > area_a/dune << EOF
  > (scope
  >  (dir vendor)
  >  (packages pkg_x))
  > (executable
  >  (name main_a)
  >  (libraries pkg_x.from_a))
  > EOF

  $ cat > area_a/main_a.ml << EOF
  > let () = print_endline Lib_x_a.msg
  > EOF

pkg_x should be visible in area_a:
  $ dune exec ./area_a/main_a.exe
  lib_x from area_a

pkg_y should NOT be visible in area_a (it was not exposed by area_a's scope):
  $ cat > area_a/dune << EOF
  > (scope
  >  (dir vendor)
  >  (packages pkg_x))
  > (executable
  >  (name main_a)
  >  (libraries pkg_y.from_a))
  > EOF

  $ cat > area_a/main_a.ml << EOF
  > let () = print_endline Lib_y_a.msg
  > EOF

  $ dune build ./area_a/main_a.exe 2>&1 | head -8
  File "area_a/dune", line 6, characters 12-24:
  6 |  (libraries pkg_y.from_a))
                  ^^^^^^^^^^^^
  Error: Library "pkg_y.from_a" not found.
  -> required by
     _build/default/area_a/.main_a.eobjs/native/dune__exe__Main_a.cmx
  -> required by _build/default/area_a/main_a.exe
  [1]

=============================================================================
Test: Executable in area_b should see pkg_y but NOT pkg_x
=============================================================================

  $ cat > area_b/dune << EOF
  > (scope
  >  (dir vendor)
  >  (packages pkg_y))
  > (executable
  >  (name main_b)
  >  (libraries pkg_y.from_b))
  > EOF

  $ cat > area_b/main_b.ml << EOF
  > let () = print_endline Lib_y_b.msg
  > EOF

pkg_y should be visible in area_b:
  $ dune exec ./area_b/main_b.exe
  lib_y from area_b

pkg_x should NOT be visible in area_b (it was not exposed by area_b's scope):
  $ cat > area_b/dune << EOF
  > (scope
  >  (dir vendor)
  >  (packages pkg_y))
  > (executable
  >  (name main_b)
  >  (libraries pkg_x.from_b))
  > EOF

  $ cat > area_b/main_b.ml << EOF
  > let () = print_endline Lib_x_b.msg
  > EOF

  $ dune build ./area_b/main_b.exe 2>&1 | head -8
  File "area_b/dune", line 6, characters 12-24:
  6 |  (libraries pkg_x.from_b))
                  ^^^^^^^^^^^^
  Error: Library "pkg_x.from_b" not found.
  -> required by
     _build/default/area_b/.main_b.eobjs/native/dune__exe__Main_b.cmx
  -> required by _build/default/area_b/main_b.exe
  [1]

=============================================================================
Test: Duplicate packages without scope stanzas should still error
=============================================================================

Create a fresh project with duplicate package names but NO scope stanzas:

  $ mkdir -p dup_test/vendor_a dup_test/vendor_b

  $ cat > dup_test/dune-project <<EOF
  > (lang dune 3.18)
  > (name dup_test)
  > (package (name dup_test))
  > EOF

  $ cat > dup_test/vendor_a/dune-project <<EOF
  > (lang dune 3.18)
  > (name dup_pkg)
  > (package (name dup_pkg))
  > EOF

  $ cat > dup_test/vendor_a/dune <<EOF
  > (library
  >  (name dup_lib)
  >  (public_name dup_pkg))
  > EOF

  $ cat > dup_test/vendor_a/dup_lib.ml <<EOF
  > let x = "from vendor_a"
  > EOF

  $ cat > dup_test/vendor_b/dune-project <<EOF
  > (lang dune 3.18)
  > (name dup_pkg)
  > (package (name dup_pkg))
  > EOF

  $ cat > dup_test/vendor_b/dune <<EOF
  > (library
  >  (name dup_lib)
  >  (public_name dup_pkg))
  > EOF

  $ cat > dup_test/vendor_b/dup_lib.ml <<EOF
  > let x = "from vendor_b"
  > EOF

  $ cat > dup_test/dune <<EOF
  > (executable
  >  (name main)
  >  (libraries dup_pkg))
  > EOF

  $ cat > dup_test/main.ml <<EOF
  > let () = print_endline Dup_lib.x
  > EOF

Without scope stanzas, duplicate packages should still cause an error:

  $ dune build ./dup_test/main.exe 2>&1 | head -5
  Error: The package "dup_pkg" is defined more than once:
  - dup_test/vendor_b/dune-project:3
  - dup_test/vendor_a/dune-project:3
  [1]
