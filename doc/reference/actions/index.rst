#########
 Actions
#########

.. highlight:: dune

``(action ...)`` fields describe user actions.

User actions are always run from the same subdirectory of the current
build context as the ``dune`` file they are defined in, so for instance,
an action defined in ``src/foo/dune`` will be run from
``$build/<context>/src/foo``.

The argument of ``(action ...)`` fields is a small DSL that's
interpreted by Dune directly and doesn't require an external shell. All
atoms in the DSL support :doc:`/concepts/variables`. Moreover, you don't
need to specify dependencies explicitly for the special
``%{<kind>:...}`` forms; these are recognized and automatically handled
by Dune.

The DSL is currently quite limited, so if you want to do something
complicated, it's recommended to write a small OCaml program and use the
DSL to invoke it. You can use `shexp
<https://github.com/janestreet/shexp>`__ to write portable scripts or
:ref:`configurator` for configuration related tasks. You can also use
:ref:`dune-action-plugin` to express program dependencies directly in
the source code.

The following constructions are available:

.. toctree::
   :caption: Running commands

   run
   system
   bash
   dynamic-run
   chdir
   setenv
   with-accepted-exit-codes

.. toctree::
   :caption: Input and output

   echo
   with-outputs-to
   with-stdin-from
   ignore-outputs
   cat
   copy
   copy#
   write-file
   pipe-outputs

.. toctree::
   :caption: Comparing files

   diff
   diffq
   cmp

.. toctree::
   :caption: Control structures

   progn
   concurrent
   no-infer

Note: expansion of the special ``%{<kind>:...}`` is done relative to the
current working directory of the DSL being executed. So for instance, if
you have this action in a ``src/foo/dune``:

.. code:: dune

   (action (chdir ../../.. (echo %{dep:dune})))

Then ``%{dep:dune}`` will expand to ``src/foo/dune``. When you run
various tools, they often use the filename given on the command line in
error messages. As a result, if you execute the command from the
original directory, it will only see the basename.

To understand why this is important, let's consider this ``dune`` file
living in ``src/foo``:

.. code::

   (rule
    (target blah.ml)
    (deps blah.mll)
    (action
     (run ocamllex -o %{target} %{deps})))

Here the command that will be executed is:

.. code:: console

   $ ocamllex -o blah.ml blah.mll

And it will be executed in ``_build/<context>/src/foo``. As a result, if
there is an error in the generated ``blah.ml`` file, it will be reported
as:

.. code::

   File "blah.ml", line 42, characters 5-10:
   Error: ...

Which can be a problem, as your editor might think that ``blah.ml`` is
at the root of your project. Instead, this is a better way to write it:

.. code::

   (rule
    (target blah.ml)
    (deps blah.mll)
    (action
     (chdir %{workspace_root}
      (run ocamllex -o %{target} %{deps}))))
