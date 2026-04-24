Comprehensive test of scope stanza semantics

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > EOF

Create a vendored directory with multiple packages and libraries:

  $ mkdir -p vendor.1.0.0/lib_a1 vendor.1.0.0/lib_a2 vendor.1.0.0/lib_b

  $ cat > vendor.1.0.0/dune-project << EOF
  > (lang dune 3.22)
  > (package (name pkg_a))
  > (package (name pkg_b))
  > EOF

Package A has two libraries (in separate dirs to avoid module conflicts):
  $ cat > vendor.1.0.0/lib_a1/dune << EOF
  > (library
  >  (name lib_a1)
  >  (public_name pkg_a.lib1))
  > EOF

  $ cat > vendor.1.0.0/lib_a1/lib_a1.ml << EOF
  > let name = "lib_a1"
  > EOF

  $ cat > vendor.1.0.0/lib_a2/dune << EOF
  > (library
  >  (name lib_a2)
  >  (public_name pkg_a.lib2))
  > EOF

  $ cat > vendor.1.0.0/lib_a2/lib_a2.ml << EOF
  > let name = "lib_a2"
  > EOF

Package B has one library:
  $ cat > vendor.1.0.0/lib_b/dune << EOF
  > (library
  >  (name lib_b)
  >  (public_name pkg_b.lib))
  > EOF

  $ cat > vendor.1.0.0/lib_b/lib_b.ml << EOF
  > let name = "lib_b"
  > EOF

=============================================================================
Test 1: Expose all packages - all libraries visible
=============================================================================

  $ cat > dune << EOF
  > (scope
  >  (dir vendor.1.0.0)
  >  (packages pkg_a pkg_b))
  > (executable
  >  (name main)
  >  (libraries pkg_a.lib1 pkg_a.lib2 pkg_b.lib))
  > EOF

  $ cat > main.ml << EOF
  > let () = Printf.printf "%s %s %s\n" Lib_a1.name Lib_a2.name Lib_b.name
  > EOF

  $ dune exec ./main.exe
  lib_a1 lib_a2 lib_b

=============================================================================
Test 2: Only expose pkg_a - all libraries from pkg_a visible
=============================================================================

  $ cat > dune << EOF
  > (scope
  >  (dir vendor.1.0.0)
  >  (packages pkg_a))
  > (executable
  >  (name main)
  >  (libraries pkg_a.lib1 pkg_a.lib2))
  > EOF

  $ cat > main.ml << EOF
  > let () = Printf.printf "%s %s\n" Lib_a1.name Lib_a2.name
  > EOF

  $ dune exec ./main.exe
  lib_a1 lib_a2

Package B should NOT be accessible:
  $ cat > dune << EOF
  > (scope
  >  (dir vendor.1.0.0)
  >  (packages pkg_a))
  > (executable
  >  (name main)
  >  (libraries pkg_b.lib))
  > EOF

  $ dune build main.exe 2>&1 | head -5
  File "dune", line 6, characters 12-21:
  6 |  (libraries pkg_b.lib))
                  ^^^^^^^^^
  Error: Library "pkg_b.lib" not found.
  -> required by _build/default/.main.eobjs/native/dune__exe__Main.cmx
  [1]

=============================================================================
Test 3: Private libraries are not visible through scope
=============================================================================

Add a private library to vendor directory:
  $ mkdir -p vendor.1.0.0/lib_private
  $ cat > vendor.1.0.0/lib_private/dune << EOF
  > (library
  >  (name lib_private))
  > EOF

  $ cat > vendor.1.0.0/lib_private/lib_private.ml << EOF
  > let name = "private"
  > EOF

Private library should not be accessible even when exposing all packages:
  $ cat > dune << EOF
  > (scope
  >  (dir vendor.1.0.0)
  >  (packages pkg_a pkg_b))
  > (executable
  >  (name main)
  >  (libraries lib_private))
  > EOF

  $ dune build main.exe 2>&1 | head -5
  File "dune", line 6, characters 12-23:
  6 |  (libraries lib_private))
                  ^^^^^^^^^^^
  Error: Library "lib_private" not found.
  -> required by _build/default/.main.eobjs/native/dune__exe__Main.cmx
  [1]

=============================================================================
Test 4: Non-existent package - error at scope stanza
=============================================================================

  $ cat > dune << EOF
  > (scope
  >  (dir vendor.1.0.0)
  >  (packages nonexistent))
  > (executable
  >  (name main)
  >  (libraries pkg_a.lib1))
  > EOF

  $ cat > main.ml << EOF
  > let () = ()
  > EOF

Invalid packages are detected early:
  $ dune build main.exe 2>&1 | head -5
  File "dune", lines 2-3, characters 1-43:
  2 |  (dir vendor.1.0.0)
  3 |  (packages nonexistent))
  Error: The following packages are not available in directory "vendor.1.0.0":
  nonexistent
  [1]

=============================================================================
Test 5: Ordered set language - :standard exposes all packages
=============================================================================

  $ cat > dune << EOF
  > (scope
  >  (dir vendor.1.0.0)
  >  (packages :standard))
  > (executable
  >  (name main)
  >  (libraries pkg_a.lib1 pkg_a.lib2 pkg_b.lib))
  > EOF

  $ cat > main.ml << EOF
  > let () = Printf.printf "%s %s %s\n" Lib_a1.name Lib_a2.name Lib_b.name
  > EOF

  $ dune exec ./main.exe
  lib_a1 lib_a2 lib_b

=============================================================================
Test 6: Ordered set language - :standard \ exclusion
=============================================================================

  $ cat > dune << EOF
  > (scope
  >  (dir vendor.1.0.0)
  >  (packages :standard \ pkg_b))
  > (executable
  >  (name main)
  >  (libraries pkg_a.lib1 pkg_a.lib2))
  > EOF

  $ cat > main.ml << EOF
  > let () = Printf.printf "%s %s\n" Lib_a1.name Lib_a2.name
  > EOF

  $ dune exec ./main.exe
  lib_a1 lib_a2

pkg_b should be excluded:
  $ cat > dune << EOF
  > (scope
  >  (dir vendor.1.0.0)
  >  (packages :standard \ pkg_b))
  > (executable
  >  (name main)
  >  (libraries pkg_b.lib))
  > EOF

  $ dune build main.exe 2>&1 | head -5
  File "dune", line 6, characters 12-21:
  6 |  (libraries pkg_b.lib))
                  ^^^^^^^^^
  Error: Library "pkg_b.lib" not found.
  -> required by _build/default/.main.eobjs/native/dune__exe__Main.cmx
  [1]

=============================================================================
Test 7: Error - only immediate subdirectories allowed
=============================================================================

  $ cat > dune << EOF
  > (scope
  >  (dir vendor.1.0.0/lib_a1)
  >  (packages pkg_a))
  > EOF

  $ dune build 2>&1 | head -7
  File "dune", line 2, characters 6-25:
  2 |  (dir vendor.1.0.0/lib_a1)
            ^^^^^^^^^^^^^^^^^^^
  Error: only immediate sub-directories may be specified.
  Hint: to ignore vendor.1.0.0/lib_a1, write "(scope lib_a1)" in
  vendor.1.0.0/dune
  [1]

=============================================================================
Test 8: Error - invalid directory names
=============================================================================

  $ cat > dune << EOF
  > (scope
  >  (dir .)
  >  (packages pkg_a))
  > EOF

  $ dune build 2>&1 | head -6
  File "dune", line 2, characters 6-7:
  2 |  (dir .)
            ^
  Error: invalid sub-directory name "."
  Hint: did you mean (scope *)?
  [1]

  $ cat > dune << EOF
  > (scope
  >  (dir ..)
  >  (packages pkg_a))
  > EOF

  $ dune build 2>&1 | head -5
  File "dune", line 2, characters 6-8:
  2 |  (dir ..)
            ^^
  Error: invalid sub-directory name ".."
  [1]

=============================================================================
Test 9: Error - scope requires dune lang 3.22
=============================================================================

  $ cat > dune-project << EOF
  > (lang dune 3.21)
  > EOF

  $ cat > dune << EOF
  > (scope
  >  (dir vendor.1.0.0)
  >  (packages pkg_a))
  > EOF

  $ dune build 2>&1 | head -7
  File "dune", lines 1-3, characters 0-45:
  1 | (scope
  2 |  (dir vendor.1.0.0)
  3 |  (packages pkg_a))
  Error: 'scope' is only available since version 3.22 of the dune language.
  Please update your dune-project file to have (lang dune 3.22).
  [1]

Restore dune-project:
  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > EOF

=============================================================================
Test 10: Duplicate scope stanzas - error
=============================================================================

Multiple scope stanzas targeting the same directory in the same file:
  $ cat > dune << EOF
  > (scope
  >  (dir vendor.1.0.0)
  >  (packages pkg_a))
  > (scope
  >  (dir vendor.1.0.0)
  >  (packages pkg_b))
  > EOF

  $ dune build 2>&1 | head -5
  File "dune", lines 5-6, characters 1-37:
  5 |  (dir vendor.1.0.0)
  6 |  (packages pkg_b))
  Error: may not set the "scope" stanza more than once
  [1]
