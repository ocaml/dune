Test that cram path rewriting handles Windows-style paths.

On Windows, paths like C:\ocaml\lib contain colons that conflict with
BUILD_PATH_PREFIX_MAP's use of colon as an entry separator. Additionally,
Windows absolute paths (e.g. C:\foo) need to be matched by the path regex.

See https://github.com/ocaml/dune/issues/10176

Create a test that uses cygpath to get the native Windows path of the root
(with a drive letter like C:\...), adds it to BUILD_PATH_PREFIX_MAP, and
verifies the path gets rewritten in output.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ cat >t1.t <<'EOF'
  > Get a native Windows path with a drive letter colon:
  > 
  >   $ native_root=$(cygpath -m /)
  >   $ export BUILD_PATH_PREFIX_MAP="/NATIVEPATH=$native_root:$BUILD_PATH_PREFIX_MAP"
  >   $ echo "path is $native_root/something"
  >   path is /NATIVEPATH/something
  > EOF

On Windows, the colon in the drive letter (e.g. C:) causes decode_map to crash
because it uses colon as the entry separator. This should succeed without error,
but currently it does not:

  $ dune runtest 2>&1 | grep -c "Cannot decode build prefix map"
  1
  [1]
