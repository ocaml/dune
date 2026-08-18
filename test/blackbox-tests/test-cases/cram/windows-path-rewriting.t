Test that cram path rewriting handles Windows-style paths.

On Windows, paths like C:\ocaml\lib contain colons that conflict with
BUILD_PATH_PREFIX_MAP's use of colon as an entry separator. Additionally,
Windows absolute paths (e.g. C:\foo) need to be matched by the path regex
whatever separator style the tested program uses: backslashes, forward
slashes, a mix of both, or escaped backslashes (e.g. in JSON output).

See https://github.com/ocaml/dune/issues/4017
and https://github.com/ocaml/dune/issues/4018
and https://github.com/ocaml/dune/issues/10176

  $ make_dune_project 3.23

A native Windows path with a drive letter colon can be added to the map
directly, without encoding the colon; decoding must not fail:

  $ cat >t1.t <<'EOF'
  >   $ native_root=$(cygpath -m /)
  >   $ export BUILD_PATH_PREFIX_MAP="/NATIVEPATH=$native_root:$BUILD_PATH_PREFIX_MAP"
  >   $ echo "path is $native_root/something"
  >   path is /NATIVEPATH/something
  > EOF

The mappings that Dune adds itself (with encoded drive-letter colons) still
rewrite native output, including JSON-escaped backslashes:

  $ cat >t2.t <<'EOF'
  >   $ echo "$(cygpath -w "$PWD")/tcs"
  >   $TESTCASE_ROOT/tcs
  >   $ printf '{"file": "%s\\\\file.ml"}\n' "$(cygpath -w "$PWD")"
  >   {"file": "$TESTCASE_ROOT/file.ml"}
  > EOF

Native backslash paths, forward-slash paths, mixed separators and
JSON-escaped backslashes are all rewritten:

  $ cat >t3.t <<'EOF'
  >   $ export BUILD_PATH_PREFIX_MAP="/ROOT=C:/work/test:$BUILD_PATH_PREFIX_MAP"
  >   $ echo 'C:\work\test\file.ml'
  >   /ROOT/file.ml
  >   $ echo 'C:/work/test/file.ml'
  >   /ROOT/file.ml
  >   $ echo 'C:\work/test\file.ml'
  >   /ROOT/file.ml
  >   $ echo 'C:/work\test\file.ml'
  >   /ROOT/file.ml
  >   $ printf '{"file": "C:\\\\work\\\\test\\\\file.ml"}\n'
  >   {"file": "/ROOT/file.ml"}
  >   $ printf '{"file": "C:\\\\work\\\\test\\\\n"}\n'
  >   {"file": "/ROOT/n"}
  >   $ echo 'C:\work\test\n'
  >   /ROOT/n
  > EOF

Overlapping mappings: the longest mapped root matches, and the rightmost
rule wins when several rules apply to the same path:

  $ cat >t4.t <<'EOF'
  >   $ export BUILD_PATH_PREFIX_MAP="/CROOT=C%.:/CWORK=C:/work:$BUILD_PATH_PREFIX_MAP"
  >   $ echo "C:/work/file"
  >   /CWORK/file
  >   $ echo "C:/other"
  >   /CROOT/other
  >   $ export BUILD_PATH_PREFIX_MAP="$BUILD_PATH_PREFIX_MAP:/SECOND=C:/work"
  >   $ echo "C:/work/file"
  >   /SECOND/file
  > EOF

Mapped roots containing spaces remain valid:

  $ cat >t5.t <<'EOF'
  >   $ spaced_src="C:/work/sub dir"
  >   $ export BUILD_PATH_PREFIX_MAP="/SPACED TARGET=$spaced_src:$BUILD_PATH_PREFIX_MAP"
  >   $ echo "$spaced_src/file.txt"
  >   /SPACED TARGET/file.txt
  > EOF

  $ dune runtest

A malformed map is reported as a user error, not an internal error:

  $ cat >bad.t <<'EOF'
  >   $ export BUILD_PATH_PREFIX_MAP=":/NOEQUALS"
  >   $ echo ok
  >   ok
  > EOF

  $ dune runtest bad.t 2>&1
  File "bad.t", line 1, characters 0-0:
  Error: Invalid BUILD_PATH_PREFIX_MAP: invalid key/value pair "/NOEQUALS", no
  '=' separator
  [1]
