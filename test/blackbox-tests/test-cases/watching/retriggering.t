Modify an input file during the build so that Dune interrupts the build once.

  $ . ./helpers.sh

Bad rule! You are not supposed to modify the source tree. No ice-cream for you!

  $ echo '(lang dune 3.0)' > dune-project
  $ cat > dune <<EOF
  > (rule
  >   (deps (glob_files *.txt) (sandbox none))
  >   (target result)
  >   (action (bash "\| echo %{deps} > result
  >                 "\| touch ../../new-source.txt
  > )))
  > EOF

  $ touch old-source.txt
  $ start_dune result

The build restarts because a new source file has been created at the root.

  $ dune_wait
  Success

  $ cat new-source.txt

The fact that new-source.txt appears bellow show that the build
restarted and observed the file:

  $ cat _build/default/result
  new-source.txt old-source.txt

So, we come back for the result once again and now get it.

  $ dune_wait
  Success

The second time the rule sees new-source.txt because Dune copied it to the
build directory. Note that the file watcher generates the next touch event for
new-source.txt but it is ignored because the contents of new-source.txt is the
same, i.e. the empty file.

  $ cat _build/default/result
  new-source.txt old-source.txt
  $ cat _build/default/new-source.txt


  $ stop_dune
  Success, waiting for filesystem changes...
