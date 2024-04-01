Command Line Interface
======================

This is a short overview of the commands available in Dune. Reference
documentation for each command is available through ``dune COMMAND --help``.

.. describe:: dune build

   Build the given targets, or the default ones.

.. describe:: dune cache

   Manage the shared cache of build artifacts.

   .. describe:: dune cache size

      Query the size of the Dune cache.

   .. describe:: dune cache trim

      Trim the Dune cache.

.. describe:: dune clean

   Clean the project.

.. describe:: dune coq

   Command group related to Coq.

   .. describe:: dune coq top

      Execute a Coq toplevel with the local configuration.

.. describe:: dune describe

   Describe the workspace.

   .. describe:: dune describe aliases

      Print aliases in a given directory. Works similarly to ls.

   .. describe:: dune describe env

      Print the environment of a directory.

   .. describe:: dune describe external-lib-deps

      Print out the external libraries needed to build the project. It's an
      approximated set of libraries.

   .. describe:: dune describe installed-libraries

      Print out the libraries installed on the system.

   .. describe:: dune describe opam-files

      Print information about the opam files that have been discovered.

   .. describe:: dune describe package-entries

      prints information about the entries per package.

   .. describe:: dune describe pp

      Build a given file and print the preprocessed output.

   .. describe:: dune describe rules

      Dump rules.

   .. describe:: dune describe targets

      Print targets in a given directory. Works similarly to ls.

   .. describe:: dune describe workspace

      Print a description of the workspace's structure. If some directories
      are provided, then only those directories of the workspace are
      considered.

.. describe:: dune diagnostics

   Fetch and return errors from the current build.

.. describe:: dune exec

   Execute a command in a similar environment as if installation was performed.

.. describe:: dune fmt

   Format source code.

.. describe:: dune format-dune-file

   Format ``dune`` files.

.. describe:: dune help

   Additional Dune help.

.. describe:: dune init

   Command group for initializing Dune components.

   .. describe:: dune init executable

      Initialize a binary executable.

   .. describe:: dune init library

      Initialize an OCaml library.

   .. describe:: dune init project

      Initialize a whole OCaml project.

   .. describe:: dune init test

      Initialize a test harness.

.. describe:: dune install

   Install packages defined in workspace.

.. describe:: dune installed-libraries

   Print out libraries installed on the system.

.. describe:: dune ocaml

   Command group related to OCaml.

   .. describe:: dune ocaml dump-dot-merlin

      Print Merlin configuration.

   .. describe:: dune ocaml merlin

      Command group related to Merlin.

      .. describe:: dune ocaml merlin dump-config

         Prints the entire content of the Merlin configuration for the given
         folder in a user friendly form.

      .. describe:: dune ocaml merlin start-session

         Start a Merlin configuration server.

   .. describe:: dune ocaml ocaml-merlin

      Start a Merlin configuration server.

   .. describe:: dune ocaml top

      Print a list of toplevel directives for including directories and loading
      ``.cma`` files.

   .. describe:: dune ocaml top-module

      Print a list of toplevel directives for loading a module into the toplevel.

   .. describe:: dune ocaml utop

      Load library in UTop.

.. describe:: dune ocaml-merlin

   Start a Merlin configuration server.

.. describe:: dune printenv

   Print the environment of a directory.

.. describe:: dune promotion

   Control how changes are propagated back to source code.

   .. describe:: dune promotion apply

           Promote files from the last run.

   .. describe:: dune promotion diff

           List promotions to be applied.

.. describe:: dune promote

   A command alias for ``dune promotion apply``.

.. describe:: dune rpc

   Dune's RPC mechanism. Experimental.

.. describe:: dune rules

   Dump rules.

.. describe:: dune runtest

   Run tests.

.. describe:: dune test

   A command alias for ``dune runtest``.

.. describe:: dune shutdown

   Cancel and shutdown any builds in the current workspace.

.. describe:: dune subst

   Substitute watermarks in source files.

.. describe:: dune top

   Print a list of toplevel directives for including directories and loading
   ``.cma`` files.

.. describe:: dune uninstall

   Uninstall packages defined in the workspace.

.. describe:: dune upgrade

   Upgrade projects across major Dune versions.

.. describe:: dune utop

   Load library in UTop.
