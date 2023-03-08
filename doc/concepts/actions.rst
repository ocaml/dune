User Actions
============

``(action ...)`` fields describe user actions.

User actions are always run from the same subdirectory of the current build
context as the ``dune`` file they are defined in, so for instance, an action defined
in ``src/foo/dune`` will be run from ``$build/<context>/src/foo``.

The argument of ``(action ...)`` fields is a small DSL that's interpreted by
Dune directly and doesn't require an external shell. All atoms in the DSL
support :doc:`variables`. Moreover, you don't need to specify dependencies
explicitly for the special ``%{<kind>:...}`` forms; these are recognized and
automatically handled by Dune.

The DSL is currently quite limited, so if you want to do something complicated,
it's recommended to write a small OCaml program and use the DSL to invoke it.
You can use `shexp <https://github.com/janestreet/shexp>`__ to write portable
scripts or :ref:`configurator` for configuration related tasks. You can also
use :ref:`dune-action-plugin` to express program dependencies directly in the
source code.

The following constructions are available:

- ``(run <prog> <args>)`` to execute a program. ``<prog>`` is resolved
  locally if it is available in the current workspace, otherwise it is
  resolved using the ``PATH``
- ``(dynamic-run <prog> <args>)`` to execute a program that was linked
  against ``dune-action-plugin`` library. ``<prog>`` is resolved in
  the same way as in ``run``
- ``(chdir <dir> <DSL>)`` to change the current directory
- ``(setenv <var> <value> <DSL>)`` to set an environment variable
- ``(with-<outputs>-to <file> <DSL>)`` to redirect the output to a file, where
  ``<outputs>`` is one of: ``stdout``, ``stderr`` or ``outputs`` (for both
  ``stdout`` and ``stderr``)
- ``(ignore-<outputs> <DSL>)`` to ignore the output, where
  ``<outputs>`` is one of: ``stdout``, ``stderr``, or ``outputs``
- ``(with-stdin-from <file> <DSL>)`` to redirect the input from a file
- ``(with-accepted-exit-codes <pred> <DSL>)`` specifies the list of expected exit codes
  for the programs executed in ``<DSL>``. ``<pred>`` is a predicate on integer
  values, and it's specified using the :doc:`predicate-language`. ``<DSL>`` can
  only contain nested occurrences of ``run``, ``bash``, ``system``, ``chdir``,
  ``setenv``, ``ignore-<outputs>``, ``with-stdin-from``, and
  ``with-<outputs>-to``. This action is available since Dune 2.0.
- ``(progn <DSL>...)`` to execute several commands in sequence
- ``(concurrent <DSL>...)``` to execute several commands concurrently
  and collect all resulting errors, if any.
  **Warning:** The concurrency is limited by the `-j` flag passed to Dune.
  In particular, if Dune is running with `-j 1`, these commands will actually
  run sequentially, which may cause a deadlock if they talk to each other.
- ``(echo <string>)`` to output a string on ``stdout``
- ``(write-file <file> <string>)`` writes ``<string>`` to ``<file>``
- ``(cat <file> ...)`` to sequentially print the contents of files to stdout
- ``(copy <src> <dst>)`` to copy a file. If these files are OCaml sources, you
  should follow the ``module_name.xxx.ml``
  :ref:`naming convention <merlin-filenames>` to preserve Merlin's
  functionality.
- ``(copy# <src> <dst>)`` to copy a file and add a line directive at
  the beginning
- ``(system <cmd>)`` to execute a command using the system shell: ``sh`` on Unix
  and ``cmd`` on Windows
- ``(bash <cmd>)`` to execute a command using ``/bin/bash``. This is obviously
  not very portable.
- ``(diff <file1> <file2>)`` is similar to ``(run diff <file1> <file2>)`` but
  is better and allows promotion. See :doc:`promotion` for more details.
- ``(diff? <file1> <file2>)`` is similar to ``(diff <file1>
  <file2>)`` except that ``<file2>`` should be produced by a part of the
  same action rather than be a dependency, is optional and will
  be consumed by ``diff?``.
- ``(cmp <file1> <file2>)`` is similar to ``(run cmp <file1> <file2>)`` but
  allows promotion. See :doc:`promotion` for more details.
- ``(no-infer <DSL>)`` to perform an action without inference of dependencies
  and targets. This is useful if you are generating dependencies in a way
  that Dune doesn't know about, for instance by calling an external build system.
- ``(pipe-<outputs> <DSL> <DSL> <DSL>...)`` to execute several actions (at least two)
  in sequence, filtering the ``<outputs>`` of the first command through the other
  command, piping the standard output of each one into the input of the next.
  This action is available since Dune 2.7.

As mentioned, ``copy#`` inserts a line directive at the beginning of
the destination file. More precisely, it inserts the following line:

.. code:: ocaml

    # 1 "<source file name>"

Most languages recognize such lines and update their current location
to report errors in the original file rather than the
copy. This is important because the copy exists only under the ``_build``
directory, and in order for editors to jump to errors when parsing the
build system's output, errors must point to files that exist in
the source tree. In the beta versions of Dune, ``copy#`` was
called ``copy-and-add-line-directive``. However, most of time, one
wants this behavior rather than a bare copy, so it was renamed to
something shorter.

Note: expansion of the special ``%{<kind>:...}`` is done relative to the current
working directory of the DSL being executed. So for instance, if you
have this action in a ``src/foo/dune``:

.. code:: dune

    (action (chdir ../../.. (echo %{dep:dune})))

Then ``%{dep:dune}`` will expand to ``src/foo/dune``. When you run various
tools, they often use the filename given on the command line in error messages.
As a result, if you execute the command from the original directory, it will
only see the basename.

To understand why this is important, let's consider this ``dune`` file living in
``src/foo``:

::

    (rule
     (target blah.ml)
     (deps   blah.mll)
     (action (run ocamllex -o %{target} %{deps})))

Here the command that will be executed is:

.. code:: bash

    ocamllex -o blah.ml blah.mll

And it will be executed in ``_build/<context>/src/foo``. As a result, if there
is an error in the generated ``blah.ml`` file, it will be reported as:

::

    File "blah.ml", line 42, characters 5-10:
    Error: ...

Which can be a problem, as your editor might think that ``blah.ml`` is at the root
of your project. Instead, this is a better way to write it:

::

    (rule
     (target blah.ml)
     (deps   blah.mll)
     (action (chdir %{workspace_root} (run ocamllex -o %{target} %{deps}))))
