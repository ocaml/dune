profile
-------

The build profile can be selected in the ``dune-workspace`` file by writing a
``(profile ...)`` stanza. For instance:

.. code:: dune

    (profile release)

Note that the command line option ``--profile`` has precedence over this stanza.

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
