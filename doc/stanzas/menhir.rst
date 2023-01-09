.. _menhir:

menhir
------

A ``menhir`` stanza is available to support the Menhir parser generator.

To use Menhir in a Dune project, the language version should be selected in the
``dune-project`` file. For example:

.. code:: scheme

  (using menhir 2.0)

This will enable support for Menhir stanzas in the current project. If the
language version is absent, Dune will automatically add this line with the
latest Menhir version once a Menhir stanza is used anywhere.

The basic form for defining menhir-git_ parsers (analogous to :ref:`ocamlyacc`)
is:

.. code:: scheme

    (menhir
     (modules <parser1> <parser2> ...)
     <optional-fields>)

``<optional-fields>`` are:

- ``(merge_into <base_name>)`` is used to define modular parsers. This
  correspond to the ``--base`` command line option of ``menhir``. With this
  option, a single parser named ``base_name`` is generated.

- ``(flags <option1> <option2> ...)`` is used to pass extra flags to Menhir.

- ``(infer <bool>)`` is used to enable Menhir with type inference. This option
  is enabled by default with Menhir language 2.0.

Menhir supports writing the grammar and automation to the ``.cmly`` file.
Therefore, if this is flag is passed to Menhir, Dune will know to introduce a
``.cmly`` target for the module.

.. _menhir-git: https://gitlab.inria.fr/fpottier/menhir
