Test how Dune handles a loop with symbolic links.

  $ echo hi > file
  $ ln -s file link
  $ mkdir test; cd test
  $ echo '(lang dune 1.12)' > dune-project

  $ cat > dune <<EOF
  > (rule
  >  (targets link)
  >  (action (bash "ln -s link link")))
  > (rule
  >  (targets t)
  >  (deps link)
  >  (action (bash "cp %{deps} t; dune_cmd stat kind %{deps}")))
  > EOF

This loop is caught immediately after running the rule that creates it.

  $ dune build t --sandbox hardlink
  File "dune", lines 1-3, characters 0-57:
  1 | (rule
  2 |  (targets link)
  3 |  (action (bash "ln -s link link")))
  Error: Error trying to read targets after a rule was run:
  - link: Cyclic symbolic link
  [1]

Let's try to create it in another way. We'll link to the external [../link],
which is circular.

  $ cat > dune <<EOF
  > (rule
  >  (deps $PWD/../link)
  >  (targets link)
  >  (action (run ln -s $PWD/../link link)))
  > (rule
  >  (targets t)
  >  (deps link)
  >  (action (bash "cp %{deps} t; dune_cmd stat kind %{deps}")))
  > EOF

  $ (cd ..; rm link; ln -s link link)

Still doesn't work: now the loop is detected when running [stat] on the
dependency.

  $ dune build t --sandbox hardlink 2>&1 | grep -v "lines 1"
  1 | (rule
  2 |  (deps $TESTCASE_ROOT/test/../link)
  3 |  (targets link)
  4 |  (action (run ln -s $TESTCASE_ROOT/test/../link link)))
  Error: File unavailable:
  $TESTCASE_ROOT/test/../link
  Cyclic symbolic link
  [1]

So, it seems like we must play dirty to create a symbolic link loop.

  $ cat > dune <<EOF
  > (rule
  >  (targets link)
  >  (action (run ln -s $PWD/../link link)))
  > (rule
  >  (deps link (sandbox none))
  >  (targets dirty-rule)
  >  (action (bash "(cd $PWD/..; rm link; ln -s link link); touch dirty-rule")))
  > (rule
  >  (targets t)
  >  (deps link dirty-rule)
  >  (action (bash "cp %{deps} t; dune_cmd stat kind %{deps}")))
  > EOF

  $ (cd ..; rm link; ln -s file link)

Finally, we get to see the error message printed out at sandbox creation.

  $ dune build t --sandbox hardlink
  File "dune", lines 8-11, characters 0-103:
   8 | (rule
   9 |  (targets t)
  10 |  (deps link dirty-rule)
  11 |  (action (bash "cp %{deps} t; dune_cmd stat kind %{deps}")))
  Error: Sandbox creation error: cannot resolve symbolic link
  "_build/default/link".
  Reason: Too many indirections; is this a cyclic symbolic link?
  [1]
