#########
 Aliases
#########

:term:`Aliases <alias>` are build targets that do not correspond to
specific files. For example, the ``runtest`` alias corresponds to
running tests.

******************
 Model and Syntax
******************

Dependencies and actions can be attached to an alias. When this alias is
requested to be built, these dependencies are built and these actions
are executed. Aliases are attached to specific directories.

In commands such as ``dune build``, the syntax to refer to the ``x``
alias is ``@x``, for example ``dune build @x``. This is why it is common
to refer to it as "the ``@x`` alias", or "attaching a rule to ``@x``".

Building ``@x`` will build ``x`` in all subdirectories of the current
directory. This is the most common case, but it is possible to restrict
this using different syntaxes:

-  ``@sub/dir/x`` will build ``x`` in ``sub/dir`` and its
   subdirectories.
-  ``@@x`` will build ``x`` in the current directory only.
-  ``@@sub/dir/x`` will build ``x`` in ``sub/dir`` only.

If ``dir`` is the directory of a :term:`build context`, it restricts the
alias to this context.

To summarize, the syntax is:

-  ``@`` (recursive) or ``@@`` (non-recursive): determine if
   subdirectories are included
-  optional :term:`build context root`: restrict to a particular
   :term:`build context`
-  optional directory: only consider this subdirectory
-  alias name

Examples:

-  ``dune build @_build/foo/runtest`` only runs the tests for the
   ``foo`` build context
-  ``dune build @runtest`` will run the tests for all build contexts

**********************
 User-Defined Aliases
**********************

It is possible to use any name for alias names; it will then be
available on the command line. For example, if a Dune file contains the
following, then ``dune build @deploy`` will execute that command.

.. code:: dune

   (rule
    (alias deploy)
    (action ./run-deployer.exe))

******************
 Built-In Aliases
******************

Some aliases are defined and managed by Dune itself.

@all
====

This alias corresponds to every known file target in a directory.

@check
======

This alias corresponds to the set of targets necessary for development
tools to work correctly. For example, it will build ``*.cmi``,
``*.cmt``, and ``*.cmti`` files so that Merlin and ``ocaml-lsp-server``
can be used in the project. It is also useful in the development loop
because it will catch compilation errors without executing expensive
operations such as linking executables.

.. _default-alias:

@default
========

This alias corresponds to the default argument for ``dune build``:
``dune build`` is equivalent to ``dune build @@default``. Similarly,
``dune build dir`` is equivalent to ``dune build @@dir/default``.

When a directory doesn't explicitly define what the ``default`` alias
means via an :doc:`files/dune/alias` stanza, the following implicit
definition is assumed:

.. code:: dune

   (alias
    (name default)
    (deps (alias_rec all)))

But if such a stanza is present in the ``dune`` file in a directory, it
will be used instead. For example, if the following is present in
``tests/dune``, ``dune build tests`` will run tests there:

.. code:: dune

   (alias
    (name default)
    (deps (alias_rec runtest)))

@doc
====

This alias builds documentation for public libraries as HTML pages.

@doc-json
=========

This alias builds documentation for public libraries as JSON files.
These are produced by ``odoc``'s option ``--as-json`` and can be
consumed by external tools.

@doc-private
============

This alias builds documentation for all libraries, both public &
private.

@fmt
====

This alias is used by formatting rules: when it is built, code
formatters will be executed (using :doc:`promotion
<../concepts/promotion>`).

``dune fmt`` is a shortcut for ``dune build @fmt --auto-promote``.

It is possible to build on top of this convention. If some actions are
manually attached to the ``fmt`` alias, they will be executed by ``dune
fmt``.

Example:

.. code:: dune

   (rule
    (with-stdout-to
     data.json.formatted
     (run jq . %{dep:data.json})))

   (rule
    (alias fmt)
    (action
     (diff data.json data.json.formatted)))

@install
========

This alias depends on the ``*.install`` files used by the :doc:`opam
integration <../explanation/opam-integration>`. In turn, these depend on
installable files.

@lint
=====

This alias runs linting tools.

@runtest
========

Actions that run tests are attached to this alias. For example this
convention is used by the ``(test)`` stanza.

``dune runtest`` is a shortcut for ``dune build @runtest``.
