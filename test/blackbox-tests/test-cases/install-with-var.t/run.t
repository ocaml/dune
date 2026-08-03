`dune install` should handle destination directories that don't exist

  $ cat > dune <<EOF
  > (install
  >  (section man)
  >  (files
  >   (man-page-a.1 as man-page-a.%{context_name}.1) ; incorrect usage!
  >   (man-page-b.1 as man1/man-page-b.%{context_name}.1)
  >   another-man-page.3))
  > EOF

  $ dune build @install
  $ dune install --prefix install --libdir $PWD/install/lib --display short
  Installing $TESTCASE_ROOT/install/lib/foo/META
  Installing $TESTCASE_ROOT/install/lib/foo/dune-package
  Installing $TESTCASE_ROOT/install/lib/foo/opam
  Installing install/man/man-page-a.default.1
  Installing install/man/man1/man-page-b.default.1
  Installing install/man/man3/another-man-page.3

  $ dune_cmd cat _build/default/foo.install | grep man
  man: [
    "_build/install/default/man/man-page-a.default.1" {"man-page-a.default.1"}
    "_build/install/default/man/man1/man-page-b.default.1"
    "_build/install/default/man/man3/another-man-page.3"

Variables can be expanded in the [dst] of files in the [bin] section. This was
once forbidden because evaluating them could cause a dependency cycle, but that
limitation was removed in #10160.

  $ cat > dune <<EOF
  > (install
  >  (section bin)
  >  (files (foobar.txt as "%{env:FOO=foobar}/foo.txt")))
  > EOF

  $ dune build @install

Expansion in [dst] also works outside of the [bin] section:

  $ cat > dune <<EOF
  > (install
  >  (section man)
  >  (files (foobar.txt as "%{env:FOO=foobar}/foo.txt")))
  > EOF

  $ dune build @install

Variables can also be expanded in the basename of [src] in the [bin] section,
even though on Windows the .exe suffix added to [dst] depends on the [src]
extension:

  $ cat > dune <<EOF
  > (install
  >  (section bin)
  >  (files (%{env:FOO=foobar.txt} as foo.txt)))
  > EOF

  $ dune build @install

This also works when the destination extension is already .exe:

  $ cat > dune <<EOF
  > (install
  >  (section bin)
  >  (files (%{env:FOO=foobar.txt} as foo.exe)))
  > EOF

  $ dune build @install

Or when the source extension is clearly not .exe:

  $ cat > dune <<EOF
  > (install
  >  (section bin)
  >  (files (%{env:FOO=foobar}.txt as foo)))
  > EOF

  $ dune build @install

The basename of [src] can be a variable even when [dst] is omitted:

  $ cat > dune <<EOF
  > (install
  >  (section bin)
  >  (files %{env:FOO=foobar}.txt))
  > EOF

  $ dune build @install

And when the basename is given literally:

  $ cat > dune <<EOF
  > (install
  >  (section bin)
  >  (files %{env:FOO=.}/foobar.txt))
  > EOF

  $ dune build @install
