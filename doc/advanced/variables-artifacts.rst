.. _variables-for-artifacts:

#########################
 Variables for Artifacts
#########################

..
   TODO(diataxis) move to :doc:`../concepts/variables`

For specific situations where one needs to refer to individual
compilation artifacts, special variables (see
:doc:`../concepts/variables`) are provided, so the user doesn't need to
be aware of the particular naming conventions or directory layout
implemented by Dune.

These variables can appear wherever a :doc:`../concepts/dependency-spec`
is expected and also inside :doc:`../reference/actions/index`. When used
inside :doc:`../reference/actions/index`, they implicitly declare a
dependency on the corresponding artifact.

The variables have the form ``%{<ext>:<path>}``, where ``<path>`` is
interpreted relative to the current directory:

-  ``cmo:<path>``, ``cmx:<path>``, and ``cmi:<path>`` expand to the
   corresponding artifact's path for the module specified by ``<path>``.
   The basename of ``<path>`` should be the name of a module as
   specified in a ``(modules)`` field.

-  ``cma:<path>`` and ``cmxa:<path>`` expands to the corresponding
   artifact's path for the library specified by ``<path>``. The basename
   of ``<path>`` should be the name of the library as specified in the
   ``(name)`` field of a ``library`` stanza (*not* its public name).

In each case, the expansion of the variable is a path pointing inside
the build context (i.e., ``_build/<context>``).
