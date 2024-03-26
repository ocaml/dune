implicit_transitive_deps
------------------------

.. describe:: (implicit_transitive_deps ...)

   Control whether transitive dependencies are made implicitly visible.

   By default, Dune allows transitive dependencies of dependencies used when
   compiling OCaml. However, this can be disabled by specifying:

   .. code:: dune

       (implicit_transitive_deps false)

   Then all dependencies directly used by a library or an executable must be
   added in the ``libraries`` field.

   We recommend users experiment with this mode and report any problems.

   Note that you must use ``threads.posix`` instead of ``threads`` when using
   this mode. This isn't an important limitation, as ``threads.vm`` is
   deprecated anyway.

   In some situations, it can be desirable to selectively preserve the behavior
   of transitive dependencies' availability a library's users. For example, if
   we define a library ``foo_more`` that extends ``foo``, we might want
   ``foo_more`` users to immediately have ``foo`` available as well. To do
   this, we must define the dependency on ``foo`` as re-exported:

   .. code:: dune

      (library
       (name foo_more)
       (libraries (re_export foo)))
