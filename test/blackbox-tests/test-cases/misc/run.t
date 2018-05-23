  $ dune runtest --display short
  File "dune", line 65, characters 21-44:
  Warning: Directory dir-that-doesnt-exist doesn't exist.
  No rule found for jbuild
          diff alias runtest
          diff alias runtest (exit 1)
  (cd _build/default && /usr/bin/diff -u result2 expected2)
  --- result2	2018-05-23 23:14:28.000000000 +0700
  +++ expected2	2018-05-23 23:14:28.000000000 +0700
  @@ -1 +1 @@
  -sub-tree/dir/b sub-tree/a
  \ No newline at end of file
  +sub-tree/a sub-tree/dir/b
  \ No newline at end of file
  [1]
