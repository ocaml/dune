@pkg-install
============

This alias is only relevant when using Dune with *package management* (see
:doc:`/dune-package-management/index`). Running ``dune build
@pkg-install`` will fetch the dependencies described in the ``depends`` field
of your ``dune-project`` (see :doc:`/reference/dune-project/package`) and build
them. It will not build your project.

Indeed, if you need to build the project, you need to use the regular ``dune
build`` command. Note that if the dependencies have not been already fetch and
downloaded, ``dune build`` will **also** take care of getting and building them.

.. note::
    ``dune build @pkg-install`` is particularly useful when you are building
    projects using per-layer caching systems, e.g., Docker images. Using this
    alias, you will be able to cache the dependencies building stage as they
    change less regularly.

If you are building the ``@pkg-install`` alias in a repository where package
management is not activated, the command will fail.

.. seealso:: :doc:`/explanation/package-management`
