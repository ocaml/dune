Checking that we compute the directory and file for dune coq top correctly

  $ dune build --display=short theories/c.vo
        coqdep theories/c.v.d
        coqdep theories/d.v.d
          coqc theories/.d.aux,theories/d.{glob,vo}
          coqc theories/.c.aux,theories/c.{glob,vo}
  $ dune build --display=short theories/b/b.vo
        coqdep theories/b/b.v.d
        coqdep theories/a/a.v.d
          coqc theories/a/.a.aux,theories/a/a.{glob,vo}
          coqc theories/b/.b.aux,theories/b/b.{glob,vo}
  $ dune coq top --toplevel=echo theories/c.v
  -topfile $TESTCASE_ROOT/_build/default/theories/c.v -R $TESTCASE_ROOT/_build/default/theories foo
  $ dune coq top --toplevel=echo theories/b/b.v
  -topfile $TESTCASE_ROOT/_build/default/theories/b/b.v -R $TESTCASE_ROOT/_build/default/theories foo
