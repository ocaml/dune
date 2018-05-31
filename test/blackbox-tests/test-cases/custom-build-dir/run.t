  $ jbuilder build foo --build-dir _foobar/ && find _foobar
  _foobar
  _foobar/default
  _foobar/default/foo
  _foobar/.digest-db
  _foobar/.universe-state
  _foobar/log
  _foobar/.to-delete-in-source-tree
  _foobar/.db

  $ jbuilder build foo --build-dir .
  Error: Invalid build directory: .
  The build directory must be an absolute path or a sub-directory of the root of the workspace.
  [1]

  $ jbuilder build foo --build-dir src/foo
  Error: Invalid build directory: src/foo
  The build directory must be an absolute path or a sub-directory of the root of the workspace.
  [1]

  $ mkdir project
  $ cp jbuild project/jbuild

Maybe this case should be supported?

  $ cd project && jbuilder build foo --build-dir ../build
  Path outside the workspace: ../build from .
  [1]

Test with build directory being an absolute path

  $ X=$PWD/build; cd project && jbuilder build foo --build-dir $X
  $ find build
  build
  build/default
  build/default/foo
  build/.digest-db
  build/.universe-state
  build/log
  build/.to-delete-in-source-tree
  build/.db
