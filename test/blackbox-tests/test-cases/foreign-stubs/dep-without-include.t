We demonstrate a strange property of the foreign stubs.

Although we introduce a dependency on all header files found in all source
directories, we do not add include directories, so they aren't accessible.

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > EOF

  $ cat >dune <<EOF
  > (include_subdirs unqualified)
  > (executable
  >  (name foo)
  >  (foreign_stubs
  >   (language c)
  >   (names bar)))
  > EOF

  $ touch foo.ml
  $ cat >bar.c <<EOF
  > void foo { }
  > EOF

  $ mkdir subdir
  $ cat >subdir/dune <<EOF
  > (rule (with-stdout-to foo.h (system "exit 1")))
  > EOF

  $ dune build foo.exe
  File "subdir/dune", line 1, characters 0-47:
  1 | (rule (with-stdout-to foo.h (system "exit 1")))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Command exited with code 1.
  [1]

The rules include the dependency on foo.h, but the include directory has to be
added manually.

  $ dune rules _build/default/bar.o | grep subdir
     (File (In_build_dir _build/default/subdir/foo.h))))
