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

The variables have the form ``%{<ext>:<path>}``, where ``<path>`` is
interpreted relative to the current directory:

- ``cmo:<path>`` and ``cmx:<path>`` expand to the corresponding OCaml
  artifact's path for the module specified by ``<path>``.

- ``cmi:<path>`` expands to the compiled interface for the specified module.

- ``melange.cmi:<path>`` expands to the Melange compiled interface for the
  specified module, including when that module is also selected for OCaml.

  .. versionadded:: 3.26

- ``cmj:<path>`` and its alias ``melange.cmj:<path>`` expand to the Melange
  compiled module for the specified module.

  .. versionadded:: 3.26

- ``cma:<path>`` and ``cmxa:<path>`` expands to the corresponding artifact's
  path for the library specified by ``<path>``. The basename of ``<path>``
  should be the name of the library as specified in the ``(name)`` field of a
  ``library`` stanza (*not* its public name).

- ``cmt:<path>`` and ``cmti:<path>`` expand to the corresponding compiled
  annotation files for the module specified by ``<path>``. These files contain
  the typed abstract syntax tree with precise location information and type
  annotations, generated with the ``-bin-annot`` flag. They are particularly
  useful for IDE tools to provide tooltips and type information.

  .. versionadded:: 3.21

- ``melange.cmt:<path>`` and ``melange.cmti:<path>`` expand to the corresponding
  Melange annotation files, including when the module is also selected for OCaml.

  .. versionadded:: 3.26

- ``melange.emit:<path>`` expands to the output directory of the
  :ref:`melange.emit stanza <melange-emit>` whose target directory is
  ``<path>``. See :ref:`melange-emit-artifact-variable` for examples.

  .. versionadded:: 3.25

The ``cmi``, ``cmt``, and ``cmti`` artifacts can be produced by OCaml or
Melange. If a module is selected for both compilation modes, these variables
refer to its OCaml artifact. Otherwise, they refer to the artifact for the mode
in which the module is selected.

The ``cmt``, ``cmj``, ``melange.cmt``, and ``melange.cmj`` variables expand to an
empty string for modules without an implementation. The ``cmti`` and
``melange.cmti`` variables select the implementation's annotation file when the
module has no explicit interface.

For module artifacts, the basename of ``<path>`` should be the name of a module
as specified in a ``(modules)`` or ``(melange.modules)`` field.

Artifact paths point inside the build context (i.e., ``_build/<context>``).
