Test that cram path rewriting handles Windows-style paths.

On Windows, paths like C:\ocaml\lib contain colons that conflict with
BUILD_PATH_PREFIX_MAP's use of colon as an entry separator. Additionally,
Windows absolute paths (e.g. C:\foo) need to be matched by the path regex
whatever separator style the tested program uses: backslashes, forward
slashes, a mix of both, or escaped backslashes (e.g. in JSON output).

See https://github.com/ocaml/dune/issues/4017
and https://github.com/ocaml/dune/issues/4018
and https://github.com/ocaml/dune/issues/10176

CR-soon Alizter: this is wrong, the paths should be rewritten. Adding a
native Windows path with a drive letter colon to the map crashes the
decoder with an internal error, output whose separators do not match the
mapped root exactly (e.g. escaped backslashes in JSON) is not rewritten,
and malformed maps are reported as internal errors instead of user errors.

  $ make_dune_project 3.23

A native Windows path with a drive letter colon crashes the decoder
(issue #10176):

  $ cat >t1.t <<'EOF'
  >   $ native_root=$(cygpath -m /)
  >   $ export BUILD_PATH_PREFIX_MAP="/NATIVEPATH=$native_root:$BUILD_PATH_PREFIX_MAP"
  >   $ echo "path is $native_root/something"
  >   path is /NATIVEPATH/something
  > EOF

  $ dune runtest t1.t > t1.out 2>&1
  [1]
  $ grep -c "Cannot decode build prefix map" t1.out
  1

Output containing escaped backslashes (JSON) is not rewritten to
$TESTCASE_ROOT (issue #4017):

  $ cat >t2.t <<'EOF'
  >   $ printf '{"file": "%s\\\\file.ml"}\n' "$(cygpath -w "$PWD")"
  >   {"file": "$TESTCASE_ROOT/file.ml"}
  > EOF

  $ dune runtest t2.t > t2.out 2>&1
  [1]
  $ grep -c 'File "t2.t"' t2.out
  1

A malformed map is reported as an internal error instead of a user error:

  $ cat >bad.t <<'EOF'
  >   $ export BUILD_PATH_PREFIX_MAP=":/NOEQUALS"
  >   $ echo ok
  >   ok
  > EOF

  $ dune runtest bad.t > bad.out 2>&1
  [1]
  $ grep -c "Cannot decode build prefix map" bad.out
  1
