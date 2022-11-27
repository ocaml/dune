Modify an input file during the build so that Dune interrupts the build once.

  $ . ./helpers.sh

Bad rule! You are not supposed to modify the source tree. No ice-cream for you!

  $ mkdir test
  $ cd test

  $ echo '(lang dune 3.0)' > dune-project
  $ cat > dune <<EOF
  > (rule
  >  (deps (glob_files *.txt) (sandbox none))
  >  (alias default)
  >  (action (system "\| echo "I'm seeing: %{deps}" >> ../../../output
  >                  "\| touch ../../new-source.txt
  > )))
  > EOF

  $ touch old-source.txt
  $ start_dune

  $ build .
  Success

We can see in the output below that the rule ran exactly twice. Note that the
file watcher generates the next touch event for new-source.txt but it
is ignored because the contents of new-source.txt is the same,
i.e. the empty file.

  $ cat ../output
  I'm seeing: old-source.txt
  I'm seeing: new-source.txt old-source.txt

  $ stop_dune
  Success, waiting for filesystem changes...
