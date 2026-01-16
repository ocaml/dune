include_subdirs
---------------

The ``include_subdirs`` stanza is used to control how Dune considers
subdirectories of the current directory. The syntax is as follows:

.. code:: dune

     (include_subdirs <mode>)

Where ``<mode>`` maybe be one of:

- ``no``, the default
- ``unqualified``
- ``qualified``

.. important::
  It's not allowed for a subdirectory of a directory with
  ``(include_subdirs <x>)`` (where ``<x>`` is not ``no``) to contain one of the
  following stanzas:

    - ``library``
    - ``executable(s)``
    - ``test(s)``


``no`` (the default)
====================

When the ``include_subdirs`` stanza isn't present or ``<mode>`` is ``no``, Dune
considers subdirectories independent.


``unqualified``
===============

When ``<mode>`` is ``unqualified``, Dune will assume that the current
directory's subdirectories are part of the same group of directories. In
particular, Dune will simultaneously scan all these directories when looking
for OCaml/Reason files. This allows you to split
:doc:`/reference/dune/library`, :doc:`/reference/dune/executable` and
:doc:`/reference/dune/test` source files among several directories.

.. note::

  ``unqualified`` means that modules in subdirectories are seen as if they were
  all in the same directory. In particular, you cannot have two modules with
  the same name in two different directories.


``qualified``
===============

When ``<mode>`` is ``qualified``, subdirectories are part of the module
hierarchy. In the source tree, files in each subdirectory will be grouped into
submodules of the :doc:`/reference/dune/library`,
:doc:`/reference/dune/executable` or :doc:`/reference/dune/test` module group,
mirroring the directory structure.

Subdirectories are included recursively. However, recursion will stop when
encountering a subdirectory that contains another ``include_subdirs`` stanza.

.. tip::

   The :doc:`/reference/dune/ocamllex`, :doc:`/reference/dune/ocamlyacc` and
   :doc:`/reference/dune/menhir` stanzas must be defined in a ``dune`` file
   next to their corresponding source files, even when the directory group root
   is an ancestor.

Module group interfaces
^^^^^^^^^^^^^^^^^^^^^^^^

At each level of the source tree, Dune generates the module interface with
aliases for all its sub-modules with a capitalized name of the directory.

- In ``app.ml``, ``sub/other.ml`` is accessible at ``Sub.Other``:

.. code::

    dune
    app.ml
    sub
    └── other.ml


Group interfaces are configurable similarly to the
:doc:`/reference/dune/library` stanza "library interface".

- ``sub/sub.ml`` defines the module interface for the modules inside ``sub/``:

.. code::

    dune
    app.ml
    sub
    ├── sub.ml
    └── other.ml

.. warning::

   Currently :doc:`/reference/dune/menhir` stanzas may not be used as a module
   group interface due to a `Dune issue
   <https://github.com/ocaml/dune/issues/8989>`_.

