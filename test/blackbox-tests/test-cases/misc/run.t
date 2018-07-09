  $ dune runtest --display short
  File "dune", line 44, characters 19-42:
  Warning: Directory dir-that-doesnt-exist doesn't exist.
          diff alias runtest
          diff alias runtest (exit 1)
  (cd _build/default && /usr/bin/diff -u result expected)
  --- result	2018-07-09 16:03:03.123914026 +0100
  +++ expected	2018-07-09 16:03:03.124914029 +0100
  @@ -1 +1 @@
  -c.txt b.txt a.txt dune
  \ No newline at end of file
  +dune a.txt b.txt c.txt
  \ No newline at end of file
          diff alias runtest (exit 1)
  (cd _build/default && /usr/bin/diff -u result2 expected2)
  --- result2	2018-07-09 16:03:03.124914029 +0100
  +++ expected2	2018-07-09 16:03:03.124914029 +0100
  @@ -1 +1 @@
  -sub-tree/dir/b sub-tree/a
  \ No newline at end of file
  +sub-tree/a sub-tree/dir/b
  \ No newline at end of file
  [1]
