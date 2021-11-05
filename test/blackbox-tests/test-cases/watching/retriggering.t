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
  $ start_dune

The build restarts because a new source file has been created.

  $ build result
  Restart (. changed)

  $ cat new-source.txt

So, we come back for the result once again and now get it.

  $ build result
  Success

  $ cat _build/default/result
  new-source.txt old-source.txt
  $ cat _build/default/new-source.txt

Some notes explaining what's going on below:

* The first time the rule is executed, it sees no *.txt files.

* The build is restarted because dir_contents for "." changed.

* The second time the rule sees new-source.txt because Dune copied it to the
| build directory.

* The file watcher generates the next touch event for new-source.txt but it is
| ignored because the contents of new-source.txt (empty file) is the same.

  $ stop_dune
  waiting for inotify sync
  waited for inotify sync
  waiting for inotify sync
  waited for inotify sync
  Success, waiting for filesystem changes...
