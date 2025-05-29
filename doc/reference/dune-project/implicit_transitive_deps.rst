implicit_transitive_deps
------------------------

.. describe:: (implicit_transitive_deps <setting>)

   Control whether transitive dependencies are made implicitly visible during
   compilation.

   ``<setting>`` is one of:

   - ``true`` makes transitive dependencies implicitly visible. This is the
     default.

   - ``false`` only listed dependencies are visible. If the ``-H`` flag is
     supported by the compiler (OCaml version >= 5.2) and Dune language version
     is >= 1.17, Dune will pass the flag to the compiler, which avoids some
     corner cases (see below).

   - ``false-if-hidden-includes-supported`` only listed dependencies are visible
     if the compiler supports the ``-H`` flag. Otherwise (OCaml version < 5.2),
     the setting is ignored and all transitive dependencies are made visible.
     Introduced in Dune 3.20.

   .. code:: dune

       (implicit_transitive_deps false)

   Then all dependencies directly used by a library or an executable must be
   added in the ``libraries`` field.

   We recommend users experiment with this mode and report any problems.

   Note that if ``-H`` flag is not being used, you must use ``threads.posix``
   instead of ``threads`` when using this mode. This isn't an important
   limitation, as ``threads.vm`` is deprecated anyway.

   In some situations, it can be desirable to selectively preserve the behavior
   of transitive dependencies' availability a library's users. For example, if
   we define a library ``foo_more`` that extends ``foo``, we might want
   ``foo_more`` users to immediately have ``foo`` available as well. To do
   this, we must define the dependency on ``foo`` as re-exported:

   .. code:: dune

      (library
       (name foo_more)
       (libraries (re_export foo)))
