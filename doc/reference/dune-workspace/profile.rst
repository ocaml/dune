profile
-------

The build profile can be selected in the ``dune-workspace`` file by writing a
``(profile ...)`` stanza. For instance:

.. code:: dune

    (profile release)

Note that the command line option ``--profile`` has precedence over this stanza.

Profile Names and Environment Stanzas
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Profiles do not need to be defined before they are selected. Any valid profile
name can be passed to ``--profile`` or used in a workspace ``(profile ...)``
field. The selected profile name is then used to choose matching entries in
:doc:`/reference/dune/env` stanzas.

For example, this workspace context selects the profile ``dbg``:

.. code:: dune

    (context
     (default
      (name dbg)
      (profile dbg)))

A ``dune`` file in the project can provide settings for that profile with:

.. code:: dune

    (env
     (dbg
      (flags (:standard -w +a))))

Standard Profiles
~~~~~~~~~~~~~~~~~

The two standard profiles are ``dev`` and ``release``.

``dev`` is the default profile when none is selected explicitly. It is intended
for interactive development and fast feedback. It enables development-oriented
defaults such as treating OCaml warnings as errors and compiling local modules
with ``-opaque`` when supported by the compiler. ``-opaque`` can make
incremental rebuilds faster by reducing dependencies on implementation details,
but it can also reduce cross-module optimization. The default value of
``%{inline_tests}`` is ``enabled`` in this profile.

``release`` is the profile used for opam releases and by commands such as
``dune build -p <package>``. It disables development-only defaults such as
warnings-as-errors and ``-opaque``. The default value of ``%{inline_tests}`` is
``disabled`` in this profile, so inline-test PPXs and backends can compile out
test code unless an :doc:`/reference/dune/env` stanza overrides it. This does
not disable ordinary ``test`` or ``tests`` stanzas. Use ``release`` for
artifacts that will be distributed or published.

User-defined profiles are not aliases for ``dev`` or ``release``. They select
matching :doc:`/reference/dune/env` stanzas by name and otherwise use Dune's
defaults for profiles that are neither ``dev`` nor ``release``.

.. _default-ocaml-flags:

Default OCaml Flags
~~~~~~~~~~~~~~~~~~~

For projects using Dune language 3.21 or later, Dune's default OCaml flags are:

- all profiles add ``-g`` to the default ``ocamlc_flags`` and
  ``ocamlopt_flags``;
- the ``dev`` profile adds ``-short-paths -keep-locs -warn-error +a`` to the
  default common ``flags``;
- ``release`` and user-defined profiles add no default common ``flags``;
- no profile adds optimization flags such as ``-O2`` or ``-O3`` by default.

These defaults are the ``:standard`` set for the corresponding flag fields.
They can be extended with ``(:standard ...)`` or replaced by omitting
``:standard``. For example, ``(ocamlc_flags ())`` and ``(ocamlopt_flags ())``
remove the default ``-g`` flags.

For projects using older Dune language versions, the ``dev`` profile uses
Dune's historical warning set. See the ``%{dune-warnings}`` variable in
:doc:`/concepts/variables` for details.
