deprecated_library_name
-----------------------

The ``deprecated_library_name`` stanza enables redirecting an old deprecated
name after a library has been renamed. It's syntax is as follows:

.. code:: dune

    (deprecated_library_name
     (old_public_name <name>)
     (new_public_name <name>))

When a developer uses the old public name in a list of library dependencies, it
will be transparently replaced by the new name. Note that it's not necessary for
the new name to exist at definition time, as it is only resolved at the point
where the old name is used.

The ``old_public_name`` can also be one of the names declared in the
``deprecated_package_names`` field of the package declaration in the
``dune-project`` file. In this case, the "old" library is understood to be a
library whose name is not prefixed by the package name. Such a library cannot be
defined in Dune, but other build systems allow it. This feature is meant to help
migration from those systems.
