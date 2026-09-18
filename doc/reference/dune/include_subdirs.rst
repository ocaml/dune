include_subdirs
---------------

The ``include_subdirs`` stanza is used to control how Dune considers
subdirectories of the current directory. The syntax is as follows:

.. code:: dune

     (include_subdirs <mode>)

Where ``<mode>`` may be one of:

- ``no``, the default
- ``unqualified``
- ``qualified``

In ``unqualified`` and ``qualified`` mode, subdirectories are included
recursively. Recursion stops at a subdirectory that contains another
``include_subdirs`` stanza or starts a separate project with ``dune-project``.

.. important::
  Subdirectories included in a directory group must not contain the following
  stanzas:

    - ``library``
    - ``executable(s)``
    - ``test(s)``
    - ``melange.emit``

  Add ``(include_subdirs no)`` to a subdirectory's ``dune`` file to make it
  independent of the enclosing group.


``no`` (the default)
====================

By default, Dune considers subdirectories independent, unless the current
directory belongs to a group defined by an ancestor's ``include_subdirs``
stanza. An explicit ``(include_subdirs no)`` opts the current directory and its
descendants out of that enclosing group.


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

.. tip::

   The :doc:`/reference/dune/ocamllex`, :doc:`/reference/dune/ocamlyacc` and
   :doc:`/reference/dune/menhir` stanzas must be defined in a ``dune`` file
   next to their corresponding source files, even when the directory group root
   is an ancestor.

Module group interfaces
^^^^^^^^^^^^^^^^^^^^^^^^

By default, Dune generates a module named after each subdirectory, with its
first letter capitalized, containing aliases for its submodules.

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

   Using a :doc:`/reference/dune/menhir` parser as a module group interface can
   produce an invalid inferred interface due to module-name collisions. OCaml
   may report warning 63 (``erroneous-printed-signature``); see
   `issue #8989 <https://github.com/ocaml/dune/issues/8989>`_.
