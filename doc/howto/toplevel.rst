#####################################
 How to Load a Project in a Toplevel
#####################################

It is possible to use OCaml code in an interactive way, by typing an
expression, which gets evaluated and its result printed. Such a program
is called a `toplevel`, or REPL (Read-Eval-Print Loop).

The compiler distribution comes with a small REPL called simply
``ocaml``, and the community has developed enhanced versions such as
`UTop <https://github.com/ocaml-community/utop>`_.

****************************************
 Building a Specialized UTop Executable
****************************************

It is possible to generate a specialized version of UTop that embeds the
current project. To do so, use the following command:

.. code:: console

   $ dune utop

The interactive session will start with all the modules loaded.

If some of the libraries are PPX rewriters, the phrases you type in the
toplevel will be rewritten with these PPX rewriters. Similarly, PPX
derivers defined in the project will be available.

***********************************
 Loading the Project in a Toplevel
***********************************

It is also possible to load Dune projects in any toplevel. To do that,
simply execute the following in your toplevel:

.. code:: ocaml

   # #use_output "dune ocaml top";;

``dune ocaml top`` is a Dune command that builds all the libraries in
the current directory and subdirectories and outputs the relevant
toplevel directives (``#directory`` and ``#load``) to make the various
modules available in the toplevel.

***************************************
 Loading a Single Module in a Toplevel
***************************************

It's also possible to load individual modules for interactive
development. Use the following dune command:

.. code:: ocaml

   # #use_output "dune ocaml top-module foo.ml";;

This will print directives that will load ``foo.ml`` without sealing it
behind ``foo.mli``. This is particularly useful for peeking and prodding
at a module's internals.
