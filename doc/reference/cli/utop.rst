utop - Load library in UTop
===========================

Dune supports launching a `utop <https://github.com/diml/utop>`__ instance
with locally defined libraries loaded.

.. code:: console

   $ dune utop <dir> -- <args>

Where ``<dir>`` is a directory under which Dune searches (recursively) for
all libraries that will be loaded. ``<args>`` will be passed as arguments to the
``utop`` command itself. For example, ``dune utop lib -- -implicit-bindings`` will
start ``utop``, with the libraries defined in ``lib`` and implicit bindings for
toplevel expressions.

Dune also supports loading individual modules unsealed by their signatures into
the toplevel. This is accomplished by launching a toplevel and then asking dune
to return the toplevel directives needed to evaluate the module:

.. code:: console

   $ utop
   # use_output "dune ocaml top-module path/to/module.ml";;

Requirements & Limitations
--------------------------

* Utop version >= 2.0 is required for this to work.
* This subcommand only supports loading libraries. Executables aren't supported.
* Libraries that are dependencies of utop itself cannot be loaded. For example
  `Camomile <https://github.com/yoriyuki/Camomile>`__.
* Loading libraries that are defined in different directories into one ``utop``
  instance isn't possible.
