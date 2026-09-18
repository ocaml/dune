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

Renaming Directories
^^^^^^^^^^^^^^^^^^^^

.. versionadded:: 3.25

The structured form of ``include_subdirs`` uses a ``mode`` field and, in
``qualified`` mode, accepts a ``dirs`` field to rename module group interfaces
independently of source directory names:

.. code:: dune

   (include_subdirs
    (mode qualified)
    (dirs (internal as public)))

The structured form requires ``(lang dune 3.25)`` or later, including when
``dirs`` is omitted.

In this example, ``internal/leaf.ml`` becomes ``Public.Leaf`` rather than
``Internal.Leaf``. In a wrapped library named ``example``, clients refer to it as
``Example.Public.Leaf``. The source files stay in ``internal/``.

Both sides use ``/``-separated paths, not dotted module references. The
destination describes the module hierarchy and need not exist on disk.

The mapping also applies to descendants: ``internal/nested/leaf.ml`` becomes
``Public.Nested.Leaf``. To rename a nested directory as well, add a more specific
mapping:

.. code:: dune

   (include_subdirs
    (mode qualified)
    (dirs
     (internal as public)
     (internal/nested as public/exposed)))

Now ``internal/nested/leaf.ml`` becomes ``Public.Exposed.Leaf``. Mappings are
matched against the original source paths, and the longest matching source
prefix takes precedence. The destination specifies the complete replacement
path relative to the directory containing the stanza, including any renamed
parent directories.

.. note::

   The ``dirs`` field does not select which subdirectories are included.
   Directories not covered by a mapping keep their usual module names. Omitting
   ``dirs`` is equivalent to ``(include_subdirs qualified)``.

Existing group interface files do not need to be renamed. With the mappings
above, ``internal/internal.ml`` defines ``Public``, and
``internal/nested/nested.ml`` defines ``Public.Exposed``. There is no need to
rename these files to ``public.ml`` or ``exposed.ml``. Fields that refer to
modules, such as a library's ``modules`` field, use the mapped names.

Directory mappings have the following restrictions:

- ``dirs`` is only allowed with ``(mode qualified)``.
- Mappings apply only to child directories, including nested ones. Source and
  destination paths are relative to the directory containing the stanza.
- Source and destination paths must have the same number of components. A
  mapping cannot flatten the hierarchy or introduce an extra level.
- Destination components must form valid OCaml module names after capitalization.
- A source directory can have only one destination.
- Mappings cannot overwrite another module or module group, or give a module
  two implementations or two interfaces, whether handwritten or generated.
  Pairing a generated implementation with a handwritten interface (or vice
  versa) is allowed.

Variables
~~~~~~~~~

Source and destination paths support :doc:`/concepts/variables`, for example
``(internal as %{read:../config/mapping})``. A mapping can read a generated file;
changes to that file update the module namespace on subsequent builds.

.. warning::

   Keep files read by a mapping outside the qualified directory group, as in
   ``../config/mapping`` above, to avoid dependency cycles.
