
exec + watch works fine when invoked at the root level
  $ DONE_FLAG=_build/done_flag
  $ dune exec --watch ./bin/main.exe $DONE_FLAG &
  Success, waiting for filesystem changes...
  foo
  $ PID=$!

Wait for the $DONE_FLAG file to exist, then delete the file. This file is
created by the program being exec'd, so when it exists we know that it's safe to
change the code and proceed with the test.
  $ ../wait-for-file.sh $DONE_FLAG
  $ kill $PID


It's broken when invoked in a child folder
  $ cd bin && dune exec --root .. --watch ./bin/main.exe &
  Entering directory '..'
  Error: posix_spawn(): No such file or directory
  Leaving directory '..'
  $ sleep 0.5;

But it works fine without watch mode
  $ cd bin && dune exec --root .. ./bin/main.exe
  Entering directory '..'
  Leaving directory '..'
  foo
