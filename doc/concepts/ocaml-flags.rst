OCaml Flags
===========

In ``library``, ``executable``, ``executables``, and ``env`` stanzas,
you can specify OCaml compilation flags using the following fields:

- ``(flags <flags>)`` to specify shared flags passed to ``ocamlc``,
  ``ocamlopt``, and ``melc``
- ``(ocamlc_flags <flags>)`` to specify flags passed to ``ocamlc`` only
- ``(ocamlopt_flags <flags>)`` to specify flags passed to ``ocamlopt`` only
- ``(melange.flags <flags>)`` to specify flags passed to ``melc`` only,
  available since Dune 3.25 with the Melange extension enabled

Compiler-specific flags are appended to the shared ``flags``. The older
spelling ``melange.compile_flags`` remains accepted, but is deprecated in Dune
language 3.25 and later. It cannot be combined with ``melange.flags`` in the
same configuration.

For all these fields, ``<flags>`` is specified in the
:doc:`../reference/ordered-set-language`.
These fields all support ``(:include ...)`` forms.

The value of ``:standard`` depends on the selected build profile. See
:ref:`default-ocaml-flags` for the default OCaml flags Dune adds, including the
default ``-g`` in ``ocamlc_flags`` and ``ocamlopt_flags``.

The default value for ``(flags ...)`` is taken from the environment,
as a result it's recommended to write ``(flags ...)`` fields as
follows:

.. code:: dune

    (flags (:standard <my options>))
