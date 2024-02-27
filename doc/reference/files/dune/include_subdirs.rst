#################
 include_subdirs
#################

The ``include_subdirs`` stanza is used to control how Dune considers
subdirectories of the current directory. The syntax is as follows:

.. code:: dune

   (include_subdirs <mode>)

Where ``<mode>`` maybe be one of:

-  ``no``, the default
-  ``unqualified``
-  ``qualified``

When the ``include_subdirs`` stanza isn't present or ``<mode>`` is
``no``, Dune considers subdirectories independent. When ``<mode>`` is
``unqualified``, Dune will assume that the current directory's
subdirectories are part of the same group of directories. In particular,
Dune will simultaneously scan all these directories when looking for
OCaml/Reason files. This allows you to split a library between several
directories. ``unqualified`` means that modules in subdirectories are
seen as if they were all in the same directory. In particular, you
cannot have two modules with the same name in two different directories.
When ``<mode>`` is ``qualified``, each subdirectory's files will be
grouped into submodules of the library module, mirroring the directory
structure.

Note that subdirectories are included recursively; however, the
recursion will stop when encountering a subdirectory that contains
another ``include_subdirs`` stanza. Additionally, it's not allowed for a
subdirectory of a directory with ``(include_subdirs <x>)`` where ``<x>``
is not ``no`` to contain one of the following stanzas:

-  ``library``
-  ``executable(s)``
-  ``test(s)``
