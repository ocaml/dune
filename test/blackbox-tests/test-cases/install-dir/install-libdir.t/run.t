To keep this test reproducible, we use a custom opam switch prefix
  $ mkdir OPAM_PREFIX
  $ export OPAM_SWITCH_PREFIX=OPAM_PREFIX

`dune install` should handle destination directories that don't exist

  $ dune build @install
  $ dune install --prefix install --libdir $PWD/install/lib --display short 2>&1 | dune_cmd sanitize
  Installing $TESTCASE_ROOT/install/lib/foo/META
  Installing $TESTCASE_ROOT/install/lib/foo/dune-package
  Installing $TESTCASE_ROOT/install/lib/foo/foo$ext_lib
  Installing $TESTCASE_ROOT/install/lib/foo/foo.cma
  Installing $TESTCASE_ROOT/install/lib/foo/foo.cmi
  Installing $TESTCASE_ROOT/install/lib/foo/foo.cmt
  Installing $TESTCASE_ROOT/install/lib/foo/foo.cmx
  Installing $TESTCASE_ROOT/install/lib/foo/foo.cmxa
  Installing $TESTCASE_ROOT/install/lib/foo/foo.ml
  Installing $TESTCASE_ROOT/install/lib/foo/opam
  Installing $TESTCASE_ROOT/install/lib/foo/foo.cmxs
  Installing install/bin/exec
  Installing install/man/a-man-page-with-no-ext
  Installing install/man/man1/a-man-page.1
  Installing install/man/man3/another-man-page.3

Even if it is possible to ask for different libexecdir than libdir, the installed .cmxs will not be found
  $ dune install --prefix install2 --libdir $PWD/install2/lib --libexecdir $PWD/install2/libexec --display short 2>&1 | dune_cmd sanitize
  Installing $TESTCASE_ROOT/install2/lib/foo/META
  Installing $TESTCASE_ROOT/install2/lib/foo/dune-package
  Installing $TESTCASE_ROOT/install2/lib/foo/foo$ext_lib
  Installing $TESTCASE_ROOT/install2/lib/foo/foo.cma
  Installing $TESTCASE_ROOT/install2/lib/foo/foo.cmi
  Installing $TESTCASE_ROOT/install2/lib/foo/foo.cmt
  Installing $TESTCASE_ROOT/install2/lib/foo/foo.cmx
  Installing $TESTCASE_ROOT/install2/lib/foo/foo.cmxa
  Installing $TESTCASE_ROOT/install2/lib/foo/foo.ml
  Installing $TESTCASE_ROOT/install2/lib/foo/opam
  Installing $TESTCASE_ROOT/install2/libexec/foo/foo.cmxs
  Installing install2/bin/exec
  Installing install2/man/a-man-page-with-no-ext
  Installing install2/man/man1/a-man-page.1
  Installing install2/man/man3/another-man-page.3

If prefix is passed, the default for libdir is `$prefix/lib`:

  $ dune install --prefix install --dry-run --display short 2>&1 | dune_cmd sanitize
  Removing (if it exists) install/lib/foo/META
  Installing install/lib/foo/META
  Creating directory install/lib/foo
  Copying _build/install/default/lib/foo/META to install/lib/foo/META (executable: false)
  Removing (if it exists) install/lib/foo/dune-package
  Installing install/lib/foo/dune-package
  Creating directory install/lib/foo
  Copying _build/install/default/lib/foo/dune-package to install/lib/foo/dune-package (executable: false)
  Removing (if it exists) install/lib/foo/foo$ext_lib
  Installing install/lib/foo/foo$ext_lib
  Creating directory install/lib/foo
  Copying _build/install/default/lib/foo/foo$ext_lib to install/lib/foo/foo$ext_lib (executable: false)
  Removing (if it exists) install/lib/foo/foo.cma
  Installing install/lib/foo/foo.cma
  Creating directory install/lib/foo
  Copying _build/install/default/lib/foo/foo.cma to install/lib/foo/foo.cma (executable: false)
  Removing (if it exists) install/lib/foo/foo.cmi
  Installing install/lib/foo/foo.cmi
  Creating directory install/lib/foo
  Copying _build/install/default/lib/foo/foo.cmi to install/lib/foo/foo.cmi (executable: false)
  Removing (if it exists) install/lib/foo/foo.cmt
  Installing install/lib/foo/foo.cmt
  Creating directory install/lib/foo
  Copying _build/install/default/lib/foo/foo.cmt to install/lib/foo/foo.cmt (executable: false)
  Removing (if it exists) install/lib/foo/foo.cmx
  Installing install/lib/foo/foo.cmx
  Creating directory install/lib/foo
  Copying _build/install/default/lib/foo/foo.cmx to install/lib/foo/foo.cmx (executable: false)
  Removing (if it exists) install/lib/foo/foo.cmxa
  Installing install/lib/foo/foo.cmxa
  Creating directory install/lib/foo
  Copying _build/install/default/lib/foo/foo.cmxa to install/lib/foo/foo.cmxa (executable: false)
  Removing (if it exists) install/lib/foo/foo.ml
  Installing install/lib/foo/foo.ml
  Creating directory install/lib/foo
  Copying _build/install/default/lib/foo/foo.ml to install/lib/foo/foo.ml (executable: false)
  Removing (if it exists) install/lib/foo/opam
  Installing install/lib/foo/opam
  Creating directory install/lib/foo
  Copying _build/install/default/lib/foo/opam to install/lib/foo/opam (executable: false)
  Removing (if it exists) install/lib/foo/foo.cmxs
  Installing install/lib/foo/foo.cmxs
  Creating directory install/lib/foo
  Copying _build/install/default/lib/foo/foo.cmxs to install/lib/foo/foo.cmxs (executable: true)
  Removing (if it exists) install/bin/exec
  Installing install/bin/exec
  Creating directory install/bin
  Copying _build/install/default/bin/exec to install/bin/exec (executable: true)
  Removing (if it exists) install/man/a-man-page-with-no-ext
  Installing install/man/a-man-page-with-no-ext
  Creating directory install/man
  Copying _build/install/default/man/a-man-page-with-no-ext to install/man/a-man-page-with-no-ext (executable: false)
  Removing (if it exists) install/man/man1/a-man-page.1
  Installing install/man/man1/a-man-page.1
  Creating directory install/man/man1
  Copying _build/install/default/man/man1/a-man-page.1 to install/man/man1/a-man-page.1 (executable: false)
  Removing (if it exists) install/man/man3/another-man-page.3
  Installing install/man/man3/another-man-page.3
  Creating directory install/man/man3
  Copying _build/install/default/man/man3/another-man-page.3 to install/man/man3/another-man-page.3 (executable: false)

If prefix is not passed, libdir defaults to the opam-prefix/lib directory:

  $ (export OCAMLFIND_DESTDIR=/OCAMLFIND_DESTDIR
  >  dune install --dry-run --display short 2>&1 | dune_cmd sanitize
  >  dune uninstall --dry-run --display short 2>&1 | dune_cmd sanitize)
  Installing $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/META
  Creating directory $TESTCASE_ROOT/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/META to $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/META (executable: false)
  Installing $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/dune-package
  Creating directory $TESTCASE_ROOT/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/dune-package to $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/dune-package (executable: false)
  Installing $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo$ext_lib
  Creating directory $TESTCASE_ROOT/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/foo$ext_lib to $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo$ext_lib (executable: false)
  Installing $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cma
  Creating directory $TESTCASE_ROOT/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/foo.cma to $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cma (executable: false)
  Installing $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cmi
  Creating directory $TESTCASE_ROOT/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/foo.cmi to $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cmi (executable: false)
  Installing $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cmt
  Creating directory $TESTCASE_ROOT/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/foo.cmt to $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cmt (executable: false)
  Installing $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cmx
  Creating directory $TESTCASE_ROOT/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/foo.cmx to $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cmx (executable: false)
  Installing $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cmxa
  Creating directory $TESTCASE_ROOT/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/foo.cmxa to $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cmxa (executable: false)
  Installing $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.ml
  Creating directory $TESTCASE_ROOT/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/foo.ml to $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.ml (executable: false)
  Installing $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/opam
  Creating directory $TESTCASE_ROOT/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/opam to $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/opam (executable: false)
  Installing $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cmxs
  Creating directory $TESTCASE_ROOT/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/foo.cmxs to $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cmxs (executable: true)
  Installing $TESTCASE_ROOT/OPAM_PREFIX/bin/exec
  Creating directory $TESTCASE_ROOT/OPAM_PREFIX/bin
  Copying _build/install/default/bin/exec to $TESTCASE_ROOT/OPAM_PREFIX/bin/exec (executable: true)
  Installing $TESTCASE_ROOT/OPAM_PREFIX/man/a-man-page-with-no-ext
  Creating directory $TESTCASE_ROOT/OPAM_PREFIX/man
  Copying _build/install/default/man/a-man-page-with-no-ext to $TESTCASE_ROOT/OPAM_PREFIX/man/a-man-page-with-no-ext (executable: false)
  Installing $TESTCASE_ROOT/OPAM_PREFIX/man/man1/a-man-page.1
  Creating directory $TESTCASE_ROOT/OPAM_PREFIX/man/man1
  Copying _build/install/default/man/man1/a-man-page.1 to $TESTCASE_ROOT/OPAM_PREFIX/man/man1/a-man-page.1 (executable: false)
  Installing $TESTCASE_ROOT/OPAM_PREFIX/man/man3/another-man-page.3
  Creating directory $TESTCASE_ROOT/OPAM_PREFIX/man/man3
  Copying _build/install/default/man/man3/another-man-page.3 to $TESTCASE_ROOT/OPAM_PREFIX/man/man3/another-man-page.3 (executable: false)
  Removing (if it exists) $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/META
  Removing (if it exists) $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/dune-package
  Removing (if it exists) $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo$ext_lib
  Removing (if it exists) $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cma
  Removing (if it exists) $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cmi
  Removing (if it exists) $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cmt
  Removing (if it exists) $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cmx
  Removing (if it exists) $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cmxa
  Removing (if it exists) $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.ml
  Removing (if it exists) $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/opam
  Removing (if it exists) $TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cmxs
  Removing (if it exists) $TESTCASE_ROOT/OPAM_PREFIX/bin/exec
  Removing (if it exists) $TESTCASE_ROOT/OPAM_PREFIX/man/a-man-page-with-no-ext
  Removing (if it exists) $TESTCASE_ROOT/OPAM_PREFIX/man/man1/a-man-page.1
  Removing (if it exists) $TESTCASE_ROOT/OPAM_PREFIX/man/man3/another-man-page.3
  Removing directory (warn if not empty) $TESTCASE_ROOT/OPAM_PREFIX/man/man3
  Removing directory (warn if not empty) $TESTCASE_ROOT/OPAM_PREFIX/man/man1
  Removing directory (warn if not empty) $TESTCASE_ROOT/OPAM_PREFIX/man
  Removing directory (warn if not empty) $TESTCASE_ROOT/OPAM_PREFIX/lib/foo
  Removing directory (warn if not empty) $TESTCASE_ROOT/OPAM_PREFIX/bin

If only libdir is passed, binaries are installed under prefix/bin and libraries
in libdir:

  $ dune install --libdir /LIBDIR --dry-run --display short 2>&1 | dune_cmd sanitize
  > dune uninstall --libdir /LIBDIR --dry-run --display short
  Installing /LIBDIR/foo/META
  Creating directory /LIBDIR/foo
  Copying _build/install/default/lib/foo/META to /LIBDIR/foo/META (executable: false)
  Installing /LIBDIR/foo/dune-package
  Creating directory /LIBDIR/foo
  Copying _build/install/default/lib/foo/dune-package to /LIBDIR/foo/dune-package (executable: false)
  Installing /LIBDIR/foo/foo$ext_lib
  Creating directory /LIBDIR/foo
  Copying _build/install/default/lib/foo/foo$ext_lib to /LIBDIR/foo/foo$ext_lib (executable: false)
  Installing /LIBDIR/foo/foo.cma
  Creating directory /LIBDIR/foo
  Copying _build/install/default/lib/foo/foo.cma to /LIBDIR/foo/foo.cma (executable: false)
  Installing /LIBDIR/foo/foo.cmi
  Creating directory /LIBDIR/foo
  Copying _build/install/default/lib/foo/foo.cmi to /LIBDIR/foo/foo.cmi (executable: false)
  Installing /LIBDIR/foo/foo.cmt
  Creating directory /LIBDIR/foo
  Copying _build/install/default/lib/foo/foo.cmt to /LIBDIR/foo/foo.cmt (executable: false)
  Installing /LIBDIR/foo/foo.cmx
  Creating directory /LIBDIR/foo
  Copying _build/install/default/lib/foo/foo.cmx to /LIBDIR/foo/foo.cmx (executable: false)
  Installing /LIBDIR/foo/foo.cmxa
  Creating directory /LIBDIR/foo
  Copying _build/install/default/lib/foo/foo.cmxa to /LIBDIR/foo/foo.cmxa (executable: false)
  Installing /LIBDIR/foo/foo.ml
  Creating directory /LIBDIR/foo
  Copying _build/install/default/lib/foo/foo.ml to /LIBDIR/foo/foo.ml (executable: false)
  Installing /LIBDIR/foo/opam
  Creating directory /LIBDIR/foo
  Copying _build/install/default/lib/foo/opam to /LIBDIR/foo/opam (executable: false)
  Installing /LIBDIR/foo/foo.cmxs
  Creating directory /LIBDIR/foo
  Copying _build/install/default/lib/foo/foo.cmxs to /LIBDIR/foo/foo.cmxs (executable: true)
  Installing $TESTCASE_ROOT/OPAM_PREFIX/bin/exec
  Creating directory $TESTCASE_ROOT/OPAM_PREFIX/bin
  Copying _build/install/default/bin/exec to $TESTCASE_ROOT/OPAM_PREFIX/bin/exec (executable: true)
  Installing $TESTCASE_ROOT/OPAM_PREFIX/man/a-man-page-with-no-ext
  Creating directory $TESTCASE_ROOT/OPAM_PREFIX/man
  Copying _build/install/default/man/a-man-page-with-no-ext to $TESTCASE_ROOT/OPAM_PREFIX/man/a-man-page-with-no-ext (executable: false)
  Installing $TESTCASE_ROOT/OPAM_PREFIX/man/man1/a-man-page.1
  Creating directory $TESTCASE_ROOT/OPAM_PREFIX/man/man1
  Copying _build/install/default/man/man1/a-man-page.1 to $TESTCASE_ROOT/OPAM_PREFIX/man/man1/a-man-page.1 (executable: false)
  Installing $TESTCASE_ROOT/OPAM_PREFIX/man/man3/another-man-page.3
  Creating directory $TESTCASE_ROOT/OPAM_PREFIX/man/man3
  Copying _build/install/default/man/man3/another-man-page.3 to $TESTCASE_ROOT/OPAM_PREFIX/man/man3/another-man-page.3 (executable: false)
  Removing (if it exists) /LIBDIR/foo/META
  Removing (if it exists) /LIBDIR/foo/dune-package
  Removing (if it exists) /LIBDIR/foo/foo.a
  Removing (if it exists) /LIBDIR/foo/foo.cma
  Removing (if it exists) /LIBDIR/foo/foo.cmi
  Removing (if it exists) /LIBDIR/foo/foo.cmt
  Removing (if it exists) /LIBDIR/foo/foo.cmx
  Removing (if it exists) /LIBDIR/foo/foo.cmxa
  Removing (if it exists) /LIBDIR/foo/foo.ml
  Removing (if it exists) /LIBDIR/foo/opam
  Removing (if it exists) /LIBDIR/foo/foo.cmxs
  Removing (if it exists) $TESTCASE_ROOT/OPAM_PREFIX/bin/exec
  Removing (if it exists) $TESTCASE_ROOT/OPAM_PREFIX/man/a-man-page-with-no-ext
  Removing (if it exists) $TESTCASE_ROOT/OPAM_PREFIX/man/man1/a-man-page.1
  Removing (if it exists) $TESTCASE_ROOT/OPAM_PREFIX/man/man3/another-man-page.3
  Removing directory (warn if not empty) $TESTCASE_ROOT/OPAM_PREFIX/man/man3
  Removing directory (warn if not empty) $TESTCASE_ROOT/OPAM_PREFIX/man/man1
  Removing directory (warn if not empty) $TESTCASE_ROOT/OPAM_PREFIX/man
  Removing directory (warn if not empty) $TESTCASE_ROOT/OPAM_PREFIX/bin
  Removing directory (warn if not empty) /LIBDIR/foo

The DESTDIR var is supported. When set, it is prepended to the prefix.
This is the case when the prefix is implicit:

  $ DESTDIR=DESTDIR dune install --dry-run --display short 2>&1 | dune_cmd sanitize
  Installing DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo/META
  Creating directory DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/META to DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo/META (executable: false)
  Installing DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo/dune-package
  Creating directory DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/dune-package to DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo/dune-package (executable: false)
  Installing DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo$ext_lib
  Creating directory DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/foo$ext_lib to DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo$ext_lib (executable: false)
  Installing DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cma
  Creating directory DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/foo.cma to DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cma (executable: false)
  Installing DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cmi
  Creating directory DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/foo.cmi to DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cmi (executable: false)
  Installing DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cmt
  Creating directory DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/foo.cmt to DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cmt (executable: false)
  Installing DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cmx
  Creating directory DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/foo.cmx to DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cmx (executable: false)
  Installing DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cmxa
  Creating directory DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/foo.cmxa to DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cmxa (executable: false)
  Installing DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.ml
  Creating directory DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/foo.ml to DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.ml (executable: false)
  Installing DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo/opam
  Creating directory DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/opam to DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo/opam (executable: false)
  Installing DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cmxs
  Creating directory DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo
  Copying _build/install/default/lib/foo/foo.cmxs to DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/lib/foo/foo.cmxs (executable: true)
  Installing DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/bin/exec
  Creating directory DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/bin
  Copying _build/install/default/bin/exec to DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/bin/exec (executable: true)
  Installing DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/man/a-man-page-with-no-ext
  Creating directory DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/man
  Copying _build/install/default/man/a-man-page-with-no-ext to DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/man/a-man-page-with-no-ext (executable: false)
  Installing DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/man/man1/a-man-page.1
  Creating directory DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/man/man1
  Copying _build/install/default/man/man1/a-man-page.1 to DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/man/man1/a-man-page.1 (executable: false)
  Installing DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/man/man3/another-man-page.3
  Creating directory DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/man/man3
  Copying _build/install/default/man/man3/another-man-page.3 to DESTDIR$TESTCASE_ROOT/OPAM_PREFIX/man/man3/another-man-page.3 (executable: false)

But also when the prefix is explicit:

  $ DESTDIR=DESTDIR dune install --prefix prefix --dry-run --display short 2>&1 | dune_cmd sanitize
  Installing DESTDIR/prefix/lib/foo/META
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/META to DESTDIR/prefix/lib/foo/META (executable: false)
  Installing DESTDIR/prefix/lib/foo/dune-package
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/dune-package to DESTDIR/prefix/lib/foo/dune-package (executable: false)
  Installing DESTDIR/prefix/lib/foo/foo$ext_lib
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/foo$ext_lib to DESTDIR/prefix/lib/foo/foo$ext_lib (executable: false)
  Installing DESTDIR/prefix/lib/foo/foo.cma
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/foo.cma to DESTDIR/prefix/lib/foo/foo.cma (executable: false)
  Installing DESTDIR/prefix/lib/foo/foo.cmi
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/foo.cmi to DESTDIR/prefix/lib/foo/foo.cmi (executable: false)
  Installing DESTDIR/prefix/lib/foo/foo.cmt
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/foo.cmt to DESTDIR/prefix/lib/foo/foo.cmt (executable: false)
  Installing DESTDIR/prefix/lib/foo/foo.cmx
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/foo.cmx to DESTDIR/prefix/lib/foo/foo.cmx (executable: false)
  Installing DESTDIR/prefix/lib/foo/foo.cmxa
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/foo.cmxa to DESTDIR/prefix/lib/foo/foo.cmxa (executable: false)
  Installing DESTDIR/prefix/lib/foo/foo.ml
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/foo.ml to DESTDIR/prefix/lib/foo/foo.ml (executable: false)
  Installing DESTDIR/prefix/lib/foo/opam
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/opam to DESTDIR/prefix/lib/foo/opam (executable: false)
  Installing DESTDIR/prefix/lib/foo/foo.cmxs
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/foo.cmxs to DESTDIR/prefix/lib/foo/foo.cmxs (executable: true)
  Installing DESTDIR/prefix/bin/exec
  Creating directory DESTDIR/prefix/bin
  Copying _build/install/default/bin/exec to DESTDIR/prefix/bin/exec (executable: true)
  Installing DESTDIR/prefix/man/a-man-page-with-no-ext
  Creating directory DESTDIR/prefix/man
  Copying _build/install/default/man/a-man-page-with-no-ext to DESTDIR/prefix/man/a-man-page-with-no-ext (executable: false)
  Installing DESTDIR/prefix/man/man1/a-man-page.1
  Creating directory DESTDIR/prefix/man/man1
  Copying _build/install/default/man/man1/a-man-page.1 to DESTDIR/prefix/man/man1/a-man-page.1 (executable: false)
  Installing DESTDIR/prefix/man/man3/another-man-page.3
  Creating directory DESTDIR/prefix/man/man3
  Copying _build/install/default/man/man3/another-man-page.3 to DESTDIR/prefix/man/man3/another-man-page.3 (executable: false)

DESTDIR can also be passed as a command line flag.

  $ dune install --destdir DESTDIR --prefix prefix --dry-run --display short 2>&1 | dune_cmd sanitize
  Installing DESTDIR/prefix/lib/foo/META
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/META to DESTDIR/prefix/lib/foo/META (executable: false)
  Installing DESTDIR/prefix/lib/foo/dune-package
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/dune-package to DESTDIR/prefix/lib/foo/dune-package (executable: false)
  Installing DESTDIR/prefix/lib/foo/foo$ext_lib
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/foo$ext_lib to DESTDIR/prefix/lib/foo/foo$ext_lib (executable: false)
  Installing DESTDIR/prefix/lib/foo/foo.cma
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/foo.cma to DESTDIR/prefix/lib/foo/foo.cma (executable: false)
  Installing DESTDIR/prefix/lib/foo/foo.cmi
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/foo.cmi to DESTDIR/prefix/lib/foo/foo.cmi (executable: false)
  Installing DESTDIR/prefix/lib/foo/foo.cmt
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/foo.cmt to DESTDIR/prefix/lib/foo/foo.cmt (executable: false)
  Installing DESTDIR/prefix/lib/foo/foo.cmx
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/foo.cmx to DESTDIR/prefix/lib/foo/foo.cmx (executable: false)
  Installing DESTDIR/prefix/lib/foo/foo.cmxa
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/foo.cmxa to DESTDIR/prefix/lib/foo/foo.cmxa (executable: false)
  Installing DESTDIR/prefix/lib/foo/foo.ml
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/foo.ml to DESTDIR/prefix/lib/foo/foo.ml (executable: false)
  Installing DESTDIR/prefix/lib/foo/opam
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/opam to DESTDIR/prefix/lib/foo/opam (executable: false)
  Installing DESTDIR/prefix/lib/foo/foo.cmxs
  Creating directory DESTDIR/prefix/lib/foo
  Copying _build/install/default/lib/foo/foo.cmxs to DESTDIR/prefix/lib/foo/foo.cmxs (executable: true)
  Installing DESTDIR/prefix/bin/exec
  Creating directory DESTDIR/prefix/bin
  Copying _build/install/default/bin/exec to DESTDIR/prefix/bin/exec (executable: true)
  Installing DESTDIR/prefix/man/a-man-page-with-no-ext
  Creating directory DESTDIR/prefix/man
  Copying _build/install/default/man/a-man-page-with-no-ext to DESTDIR/prefix/man/a-man-page-with-no-ext (executable: false)
  Installing DESTDIR/prefix/man/man1/a-man-page.1
  Creating directory DESTDIR/prefix/man/man1
  Copying _build/install/default/man/man1/a-man-page.1 to DESTDIR/prefix/man/man1/a-man-page.1 (executable: false)
  Installing DESTDIR/prefix/man/man3/another-man-page.3
  Creating directory DESTDIR/prefix/man/man3
  Copying _build/install/default/man/man3/another-man-page.3 to DESTDIR/prefix/man/man3/another-man-page.3 (executable: false)
