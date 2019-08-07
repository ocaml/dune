.. _user-actions:

************
User actions
************

``(action ...)`` fields describe user actions.

User actions are always run from the same subdirectory of the current build
context as the dune file they are defined in. So for instance an action defined
in ``src/foo/dune`` will be run from ``$build/<context>/src/foo``.

The argument of ``(action ...)`` fields is a small DSL that is interpreted by
dune directly and doesn't require an external shell. All atoms in the DSL
support `Variables expansion`_. Moreover, you don't need to specify dependencies
explicitly for the special ``%{<kind>:...}`` forms, these are recognized and
automatically handled by dune.

The DSL is currently quite limited, so if you want to do something complicated
it is recommended to write a small OCaml program and use the DSL to invoke it.
You can use `shexp <https://github.com/janestreet/shexp>`__ to write portable
scripts or configurator_ for configuration related tasks.

The following constructions are available:

- ``(run <prog> <args>)`` to execute a program. ``<prog>`` is resolved
  locally if it is available in the current workspace, otherwise it is
  resolved using the ``PATH``
- ``(chdir <dir> <DSL>)`` to change the current directory
- ``(setenv <var> <value> <DSL>)`` to set an environment variable
- ``(with-<outputs>-to <file> <DSL>)`` to redirect the output to a file, where
  ``<outputs>`` is one of: ``stdout``, ``stderr`` or ``outputs`` (for both
  ``stdout`` and ``stderr``)
- ``(ignore-<outputs> <DSL)`` to ignore the output, where
  ``<outputs>`` is one of: ``stdout``, ``stderr`` or ``outputs``
- ``(with-stdin-from <file> <DSL>)`` to redirect the input from a file
- ``(progn <DSL>...)`` to execute several commands in sequence
- ``(echo <string>)`` to output a string on stdout
- ``(write-file <file> <string>)`` writes ``<string>`` to ``<file>``
- ``(cat <file>)`` to print the contents of a file to stdout
- ``(copy <src> <dst>)`` to copy a file
- ``(copy# <src> <dst>)`` to copy a file and add a line directive at
  the beginning
- ``(system <cmd>)`` to execute a command using the system shell: ``sh`` on Unix
  and ``cmd`` on Windows
- ``(bash <cmd>)`` to execute a command using ``/bin/bash``. This is obviously
  not very portable
- ``(diff <file1> <file2>)`` is similar to ``(run diff <file1>
  <file2>)`` but is better and allows promotion.  See `Diffing and
  promotion`_ for more details
- ``(diff? <file1> <file2>)`` is the same as ``(diff <file1>
  <file2>)`` except that it is ignored when ``<file1>`` or ``<file2>``
  doesn't exists
- ``(cmp <file1> <file2>)`` is similar to ``(run cmp <file1>
  <file2>)`` but allows promotion.  See `Diffing and promotion`_ for
  more details

As mentioned ``copy#`` inserts a line directive at the beginning of
the destination file. More precisely, it inserts the following line:

.. code:: ocaml

    # 1 "<source file name>"

Most languages recognize such lines and update their current location,
in order to report errors in the original file rather than the
copy. This is important as the copy exists only under the ``_build``
directory and in order for editors to jump to errors when parsing the
output of the build system, errors must point to files that exist in
the source tree. In the beta versions of dune, ``copy#`` was
called ``copy-and-add-line-directive``. However, most of time one
wants this behavior rather than a bare copy, so it was renamed to
something shorter.

Note: expansion of the special ``%{<kind>:...}`` is done relative to the current
working directory of the part of the DSL being executed. So for instance if you
have this action in a ``src/foo/dune``:

.. code:: scheme

    (action (chdir ../../.. (echo %{path:dune})))

Then ``%{path:dune}`` will expand to ``src/foo/dune``. When you run various
tools, they often use the filename given on the command line in error messages.
As a result, if you execute the command from the original directory, it will
only see the basename.

To understand why this is important, let's consider this dune file living in
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
is an error in the generated ``blah.ml`` file it will be reported as:

::

    File "blah.ml", line 42, characters 5-10:
    Error: ...

Which can be a problem as you editor might think that ``blah.ml`` is at the root
of your project. What you should write instead is:

::

    (rule
     (target blah.ml)
     (deps   blah.mll)
     (action (chdir %{workspace_root} (run ocamllex -o %{target} %{deps}))))
