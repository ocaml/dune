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
  $ runtest "(copy \"$PWD/dummy\")" 2>&1 | sed "s#$(pwd)#PWD#" | sed '/ *^\^*$/d' | sed '\#^File "dune.lock/foo.pkg", line 2, characters#d'
  2 | (source (copy "PWD/dummy"))
  Error:
  PWD/dummy
  does not exist

Git
  $ runtest "(fetch (url \"git+file://$PWD/dummy\"))" 2>&1 | sed "s#$(pwd)#PWD#"
  fatal: 'PWD/dummy' does not appear to be a git repository
  fatal: Could not read from remote repository.
  
  Please make sure you have the correct access rights
  and the repository exists.
  Error: Failed to run external command:
  'git ls-remote "file://PWD/dummy"'
  -> required by _build/_private/default/.pkg/foo/source
  -> required by _build/_private/default/.pkg/foo/target
  Hint: Check that this Git URL in the project configuration is correct:
  "file://PWD/dummy"

HTTP

  $ runtest "(fetch (url \"https://0.0.0.0:35000\"))" 2>&1 | sed -ne '/Error:/,$ p' | sed '/^Reason/ q' | sed "s/'[0-9]*'/X/"
  Error: failed to extract 'download'
  Reason: 'tar' failed with non-zero exit code X and output:
