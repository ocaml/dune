.. _variables-for-artifacts:

Variables for Artifacts
-----------------------

.. TODO(diataxis) move to :doc:`../concepts/variables`

For specific situations where one needs to refer to individual compilation
artifacts, special variables (see :doc:`../concepts/variables`) are provided,
so the user doesn't need to be aware of the particular naming conventions or
directory layout implemented by Dune.

These variables can appear wherever a :doc:`../concepts/dependency-spec` is
expected and also inside :doc:`../reference/actions/index`. When used inside
:doc:`../reference/actions/index`, they implicitly declare a dependency on the
corresponding artifact.

The variables have the form ``%{<ext>:<reference>}``:

- ``cmo:<reference>``, ``cmx:<reference>``, and ``cmi:<reference>`` expand to
  the corresponding artifact's path for the specified module. Starting with
  Dune language version 3.25, the final component of ``<reference>`` is the
  module's full logical path. It is rooted at the selected module tree even when
  used in a nested ``dune`` file. For example, with
  ``(include_subdirs qualified)``, ``%{cmi:Foo.Bar}`` refers to the module
  ``Foo.Bar``. Qualified references require ``(include_subdirs qualified)``.
  An optional directory prefix is interpreted relative to the current directory
  and selects another module tree, as in ``%{cmi:sub/X}``. To prevent a legacy
  source path from silently selecting a different logical module, Dune rejects
  a reference when its complete spelling is the source path of another module.
  In earlier language versions, the whole ``<reference>`` is the extensionless
  source path interpreted relative to the current directory, such as
  ``%{cmi:foo/bar}``.

- ``cma:<path>`` and ``cmxa:<path>`` expand to the corresponding artifact's
  path for the library specified by ``<path>``, interpreted relative to the
  current directory. The basename of ``<path>`` should be the name of the
  library as specified in the ``(name)`` field of a ``library`` stanza (*not*
  its public name).

- ``cmt:<reference>`` and ``cmti:<reference>`` expand to the corresponding
  compiled annotation files for the specified module. Module references are
  interpreted in the same way as for ``cmo``, ``cmx``, and ``cmi``. These files
  contain the typed abstract syntax tree with precise location information and
  type annotations, generated with the ``-bin-annot`` flag. They are
  particularly useful for IDE tools to provide tooltips and type information.

  .. versionadded:: 3.21

- ``melange.emit:<path>`` expands to the output directory of the
  :ref:`melange.emit stanza <melange-emit>` whose target directory is
  ``<path>``. See :ref:`melange-emit-artifact-variable` for examples.

  .. versionadded:: 3.25

In each case, the expansion of the variable is a path pointing inside the build
context (i.e., ``_build/<context>``).
