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
  >                 "\| sleep 0.01
  > )))
  > EOF

  $ touch old-source.txt
  $ start_dune

The build restarts because a new source file has been created at the root.

  $ build result
  Restart (. changed)

  $ cat new-source.txt

So, we come back for the result once again and now get it.

  $ build result
  Success

The second time the rule sees new-source.txt because Dune copied it to the
build directory. Note that the file watcher generates the next touch event for
new-source.txt but it is ignored because the contents of new-source.txt is the
same, i.e. the empty file.

  $ cat _build/default/result
  new-source.txt old-source.txt
  $ cat _build/default/new-source.txt


We are done.

  $ stop_dune
  waiting for inotify sync
  waited for inotify sync
  waiting for inotify sync
  waited for inotify sync
  Success, waiting for filesystem changes...
