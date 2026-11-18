run
---

.. highlight:: dune

.. describe:: (run <prog> <args>)

   Execute a program. ``<prog>`` is resolved locally if Dune knows about a
   program by that name in the current workspace, otherwise it is resolved using
   the ``PATH``.

   "Available in the current workspace" does not mean that Dune searches the
   workspace source tree for an arbitrary executable file called ``<prog>``.
   Instead, the program must be one of the binaries known to Dune's command
   lookup, for example:

   - an executable installed by a workspace package, typically via an
     :doc:`/reference/dune/executable` stanza with ``(public_name ...)`` or an
     :doc:`/reference/dune/install` stanza with ``(section bin)``;
   - an executable installed by a package of the lock directory;
   - a binary introduced by an :doc:`/reference/dune/env` stanza's
     ``(binaries ...)`` field;
   - or an executable found in ``PATH``. Note that directories of ``PATH``
     inside the build directory, such as those added by ``eval $(dune tools
     env)``, are ignored: executables built by Dune must be reached through the
     cases above so that the action depends on them.

   To run a private executable target from the source tree, refer to it by path,
   such as ``(run ./tool.exe ...)``. To make a private executable available by a
   bare command name, add it to the command lookup with an ``env`` stanza:

   .. code:: dune

      (env
       (_
        (binaries
         (./tool.exe as tool))))

   Example::

   (run capnp compile -o %{bin:capnpc-ocaml} schema.capnp)
