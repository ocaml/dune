Test the behaviour of exec watch mode when the current directory is not the
project root directory.

"dune exec --watch" works fine when invoked at the root level
  $ DONE_FLAG=_build/done_flag
  $ dune exec --watch ./bin/main.exe $DONE_FLAG &
  foo
  Success, waiting for filesystem changes...
  $ PID=$!

Wait for the $DONE_FLAG file to exist, then delete the file. This file is
created by the program being exec'd, so when it exists we know that it's safe to
change the code and proceed with the test.
  $ ../wait-for-file.sh $DONE_FLAG
  $ kill $PID
  $ wait $PID
  [130]

Perform the same test above but first enter the "bin" directory.
  $ dune clean
  $ cd bin
  $ dune exec --root .. --watch ./bin/main.exe ../$DONE_FLAG &
  Entering directory '..'
  foo
  Success, waiting for filesystem changes...
  Leaving directory '..'
  $ PID=$!
  $ cd ..
  $ ../wait-for-file.sh $DONE_FLAG
  $ kill $PID
  $ wait $PID
  [130]

Test that the behaviour is the same when not running with "--watch"
  $ cd bin && dune exec --root .. ./bin/main.exe
  Entering directory '..'
  Leaving directory '..'
  foo
