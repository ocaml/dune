  $ dune runtest --display short 2>&1 | sed "s/ cmd /  sh /"
            sh stderr,stdout
            sh stderr,stdout
          diff alias runtest
            sh both
            sh both
          diff alias runtest (exit 1)
  (cd _build/default && /usr/bin/diff -uw both.expected both)
  --- both.expected	2018-12-11 23:49:42.000000000 +0100
  +++ both	2018-12-11 23:49:42.000000000 +0100
  @@ -1,2 +1 @@
  -toto
   titi
          diff alias runtest (exit 1)
  (cd _build/default && /usr/bin/diff -uw stdout.expected stdout)
  --- stdout.expected	2018-12-11 23:49:42.000000000 +0100
  +++ stdout	2018-12-11 23:49:42.000000000 +0100
  @@ -1 +0,0 @@
  -toto
