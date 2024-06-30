OCaml Flags
===========

In ``library``, ``executable``, ``executables``, and ``env`` stanzas,
you can specify OCaml compilation flags using the following fields:

- ``(flags <flags>)`` to specify flags passed to both ``ocamlc`` and
  ``ocamlopt``
- ``(ocamlc_flags <flags>)`` to specify flags passed to ``ocamlc`` only
- ``(ocamlopt_flags <flags>)`` to specify flags passed to ``ocamlopt`` only

For all these fields, ``<flags>`` is specified in the
:doc:`../reference/ordered-set-language`.
These fields all support ``(:include ...)`` forms.

The default value for ``(flags ...)`` is taken from the environment,
as a result it's recommended to write ``(flags ...)`` fields as
follows:

.. code:: dune

    (flags (:standard <my options>))

For complete usage examples, see
:ref:`Setting the OCaml Compilation Flags Globally`,
:ref:`How to Make Warnings Non-Fatal` and
:ref:`How to Turn Specific Errors into Warnings`.
