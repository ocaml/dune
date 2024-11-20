@pkg-install
============

This alias is only relevant when using ``dune`` with *package-management*
(see :doc:`/tutorials/dune-package-management/index`). Running ``dune build
@pkg-install`` will fetch the dependencies described in the ``depends`` field
of your ``dune-project`` (see :doc:`/reference/dune-project/package`) and build them.

.. note::
    Note it will not build your project, only its dependencies. It is useful if
    you are building projects using per-layer caching systems, e.g. Docker
    images.

If you are building the ``@pkg-install`` alias in a repository where package
management is not activated, the command has no effect.

.. seealso:: :doc:`/explanation/package-management`
