Demonstrate what happens when we try to fetch from a source that doesn't exist:

  $ . ./helpers.sh

  $ make_lockdir

  $ runtest() {
  > make_lockpkg foo <<EOF
  > (build (run echo building))
  > (source $1)
  > (version dev)
  > EOF
  > build_pkg foo 2>&1
  > }

Local file system
  $ runtest "(copy \"$PWD/dummy\")" 2>&1 | sed -ne '/Error: /,$ p'
  Error: Unable to read
  $TESTCASE_ROOT/dummy
  opendir($TESTCASE_ROOT/dummy): No such file or directory

Git
  $ runtest "(fetch (url \"git+file://$PWD/dummy\"))" 2>&1 | awk '/fatal:/,/Description/'
  fatal: '$TESTCASE_ROOT/dummy' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  Internal error, please report upstream including the contents of _build/log.
  Description:


Http
A bit annoying that this test can pass by accident if there's a server running
on 35000
  $ runtest "(fetch (url \"https://0.0.0.0:35000\"))" 2>&1 | sed -ne '/Error:/,$ p'
  Error: curl returned an invalid error code 7
         
         
