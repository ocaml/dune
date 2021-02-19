To mimic the behavior of Dune < 2.8 we add rules generating and promoting
until-clean .merlin files in both source folders (root and subfolder subdir)
We also add an other promotion that should not be impacted by these changes.
  $ cat >dune <<EOF
  > (executable
  >  (name foo)
  >  (promote (until-clean)))
  > 
  > (rule
  >  (targets .merlin)
  >  (action (with-stdout-to .merlin (echo "test")))
  >  (mode (promote (until-clean))))
  > EOF

  $ mkdir subdir

  $ cat >subdir/dune <<EOF
  > (rule
  >  (targets .merlin)
  >  (action (with-stdout-to .merlin (echo "test")))
  >  (mode (promote (until-clean))))
  > EOF

Building the project will promote .merlin files and foo.exe
  $ dune build
  $ ls -a | grep -i -e .merlin -e foo.exe
  .merlin
  foo.exe

  $ ls -a subdir | grep -i -e .merlin -e foo.exe
  .merlin

Nothing happen on rebuild, rules are still in place, promoted files remain
  $ dune build --verbose 2>&1 | grep "left-over"
  [1]

  $ ls -a | grep -i -e .merlin -e foo.exe
  .merlin
  foo.exe

  $ ls -a subdir | grep -i -e .merlin -e foo.exe
  .merlin

Now we remove the rules with promotions
  $ cat >dune <<EOF
  > EOF
  $ cat >subdir/dune <<EOF
  > EOF

Next build Dune will delete the leftover .merlin but not foo.exe
  $ dune build --verbose 2>&1 | grep "left-over"
  Deleting left-over Merlin file .merlin.
  Deleting left-over Merlin file subdir/.merlin.

  $ ls -a | grep -i -e .merlin -e foo.exe
  foo.exe

  $ ls -a subdir | grep -i -e .merlin -e foo.exe
  [1]
