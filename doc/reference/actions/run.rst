run
---

.. highlight:: dune

.. describe:: (run <prog> <args>)

   Execute a program. ``<prog>`` is resolved locally if it is available in the
   current workspace, otherwise it is resolved using the ``PATH``.

   Example::

   (run capnp compile -o %{bin:capnpc-ocaml} schema.capnp)
