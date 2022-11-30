This test makes sure that inline_tests backends are functional when they are
externally installed.

First we build and use the backend locally:

  $ dune runtest dune-file
  414243

Then we install the backend and check that the "inline_tests.backend"
field is properly generated in the installed `dune-package` file:

  $ dune build dune-file/foo.install
  $ dune install foo --prefix _install 2> /dev/null
  $ grep -A8 inline_tests.backend _install/lib/foo/dune-package
   (inline_tests.backend
    (runner_libraries str)
    (flags
     inline-test-runner
     %{library-name}
     -source-tree-root
     %{workspace_root}
     -diff-cmd
     -)

Now we make sure that we can use the backend when it's available as an external
package:

  $ export OCAMLPATH=$PWD/_install/lib; dune runtest --root dune-file-user
  Entering directory 'dune-file-user'
  414243
  Leaving directory 'dune-file-user'
