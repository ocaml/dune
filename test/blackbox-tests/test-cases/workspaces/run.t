jbuild still discovers workspaces as usual

  $ jbuilder build --root jbuilder-default-name
  File "jbuild-workspace", line 1, characters 10-24:
  Error: Unknown constructor does-not-exist
  [1]

and dune ignores this file:

  $ dune build --root jbuilder-default-name
  Entering directory 'jbuilder-default-name'

dune uses a versioned file. If the version is missing, then we get an error.

  $ dune build --root dune-no-version
  File "dune-workspace", line 1, characters 0-19:
  Error: Invalid first line, expected: (lang <lang> <version>)
  [1]

analogously, jbuilder will ignore it

  $ jbuilder build --root dune-no-version
  Entering directory 'dune-no-version'

specifying the workspace file is possible:

  $ dune build --root custom-workspace --workspace custom-workspace/dune-workspace.dev
  Error: exception Sys_error("custom-workspace/dune-workspace.dev: No such file or directory")
  Backtrace:
  Raised by primitive operation at file "pervasives.ml", line 389, characters 28-54
  Called from file "src/stdune/io.ml", line 15, characters 15-35
  Called from file "src/main.ml", line 61, characters 8-36
  Called from file "src/main.ml", line 270, characters 12-56
  Called from file "bin/main.ml", line 756, characters 7-29
  Called from file "vendor/cmdliner/src/cmdliner_term.ml", line 27, characters 19-24
  Called from file "vendor/cmdliner/src/cmdliner.ml", line 106, characters 32-39
  Called from file "vendor/cmdliner/src/cmdliner.ml", line 136, characters 18-36
  Called from file "vendor/cmdliner/src/cmdliner.ml", line 251, characters 22-48
  Called from file "bin/main.ml", line 1562, characters 10-51
  [1]

Workspaces let you set custom profiles

  $ dune runtest --root custom-profile
  Info: creating file dune-project with this contents: (lang dune 1.0)
  Entering directory 'custom-profile'
  build profile: foobar

A workspace context can ve defined using an opam switch. Note that this test is
a bit limited since we can't mock opam switches.

  $ dune build --root opam --display quiet 2>&1 | grep -v "opam (internal)\|To make opam select\|OPAMSWITCH"
  Entering directory 'opam'

Workspaces also allow you to set "target" for cross compilation. This feature is
a bit hard to test since it requires mocking more than one context. But we can
see how we can set a "native" target. Which is the default.

  $ dune exec ./foo.exe --root targets-native
  Info: creating file dune-project with this contents: (lang dune 1.0)
  Entering directory 'targets-native'
  Entering directory 'targets-native'
  message from targets-native test
