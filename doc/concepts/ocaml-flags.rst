OCaml Flags
===========

In ``library``, ``executable``, ``executables``, and ``env`` stanzas,
you can specify OCaml compilation flags using the following fields:

- ``(flags <flags>)`` to specify shared flags passed to ``ocamlc`` and
  ``ocamlopt``, and to ``melc`` when ``melange.flags`` is not set
- ``(ocamlc_flags <flags>)`` to specify flags passed to ``ocamlc`` only
- ``(ocamlopt_flags <flags>)`` to specify flags passed to ``ocamlopt`` only
- ``(melange.flags <flags>)`` to specify flags passed to ``melc`` only,
  available since Dune 3.26 with the Melange extension enabled

The ``ocamlc_flags`` and ``ocamlopt_flags`` fields are appended to the shared
``flags``. The ``melange.flags`` field specifies Melange's flags
independently of ``flags``, including when it is empty. Its ``:standard``
value inherits the Melange flags from the environment, defaulting to ``-g``
when no Melange flags are configured. If ``melange.flags`` is set in an
``env`` stanza, libraries inherit that configuration. Otherwise, omitting
the field keeps using the shared ``flags`` for Melange.

The older ``melange.compile_flags`` field keeps its original behavior: its
flags are appended to the shared ``flags``, even without ``:standard``. It
is deprecated in Dune language 3.26 and later, and cannot be combined with
``melange.flags`` in the same configuration.

For all these fields, ``<flags>`` is specified in the
:doc:`../reference/ordered-set-language`.
These fields all support ``(:include ...)`` forms.

For OCaml, the value of ``:standard`` depends on the selected build profile. See
:ref:`default-ocaml-flags` for the default OCaml flags Dune adds, including the
default ``-g`` in ``ocamlc_flags`` and ``ocamlopt_flags``.

The default value for ``(flags ...)`` is taken from the environment,
as a result it's recommended to write ``(flags ...)`` fields as
follows:

.. code:: dune

    (flags (:standard <my options>))
