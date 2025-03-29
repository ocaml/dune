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

If you would like to set the ``-keywords`` flag for Ocaml 5.3 and above,
you can do so using the ``(keywords)`` field which is available in the
same stanzas as the other Ocaml flags. You can set this field in the
following ways:

- ``(keywords (version <major>.<minor>))`` to set the Ocaml version to use
  keywords from, e.g. ``(keywords (version 5.3))``.
- ``(keywords (extra <keyword1> <keyword2>))`` to add extra keywords to the
  currently in use Ocaml version, e.g. ``(keywords (extra atomic))``.
- ``(keywords (version <major>.<minor>) (extra <keyword1> <keyword2>))``
  to use the keywords from a specific version and add some extra keywords,
  e.g. ``(keywords (version 5.2) (extra effect))``.
