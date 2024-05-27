Aliases
=======

:term:`Aliases <alias>` are build targets that do not correspond to specific
files. For example, the ``runtest`` alias corresponds to running tests.

Model and Syntax
----------------

Dependencies and actions can be attached to an alias. When this alias is
requested to be built, these dependencies are built and these actions are
executed. Aliases are attached to specific directories.

In commands such as ``dune build``, the syntax to refer to the ``x`` alias is
``@x``, for example ``dune build @x``. This is why it is common to refer to it
as "the ``@x`` alias", or "attaching a rule to ``@x``".

Building ``@x`` will build ``x`` in all subdirectories of the current
directory. This is the most common case, but it is possible to restrict this
using different syntaxes:

- ``@sub/dir/x`` will build ``x`` in ``sub/dir`` and its subdirectories.
- ``@@x`` will build ``x`` in the current directory only.
- ``@@sub/dir/x`` will build ``x`` in ``sub/dir`` only.

If ``dir`` is the directory of a :term:`build context`, it restricts the alias
to this context.

To summarize, the syntax is:

- ``@`` (recursive) or ``@@`` (non-recursive): determine if subdirectories are
  included
- optional :term:`build context root`: restrict to a particular :term:`build
  context`
- optional directory: only consider this subdirectory
- alias name

Examples:

- ``dune build @_build/foo/runtest`` only runs the tests for
  the ``foo`` build context
- ``dune build @runtest`` will run the tests for all build contexts

User-Defined Aliases
--------------------

It is possible to use any name for alias names; it will then be available on
the command line. For example, if a Dune file contains the following, then
``dune build @deploy`` will execute that command.

.. code:: dune

   (rule
    (alias deploy)
    (action ./run-deployer.exe))

Built-In Aliases
----------------

Some aliases are defined and managed by Dune itself:

.. grid:: 1 3 2 3

  .. grid-item::

    .. toctree::
       :caption: Builds

       aliases/all
       aliases/default
       aliases/install

  .. grid-item::

    .. toctree::
       :caption: Checks

       aliases/check
       aliases/ocaml-index
       aliases/runtest
       aliases/fmt
       aliases/lint

  .. grid-item::

    .. toctree::
       :caption: Docs

       aliases/doc
       aliases/doc-private
       aliases/doc-json
