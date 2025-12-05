This test verifies the @pkg-install alias outputs the build progress when
--display is short or verbose.

  $ . ./helpers.sh

Add a lock file for a fake library foo:
  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > EOF

Verify that the build progress is displayed correctly
  $ dune build @pkg-install --display short
      Building foo.0.0.1

  $ dune clean

  $ dune build @pkg-install --display verbose 2>&1 | grep Building
      Building foo.0.0.1

  $ dune clean

  $ dune build @pkg-install --display quiet
