.. _dune-env:

env
---

The ``env`` stanza allows one to modify the environment. The syntax is as
follows:

.. code:: dune

     (env
      (<profile1> <settings1>)
      (<profile2> <settings2>)
      ...
      (<profilen> <settingsn>))

The first form ``(<profile> <settings>)`` that corresponds to the selected build
profile will be used to modify the environment in this directory. You can use
``_`` to match any build profile.

Fields supported in ``<settings>`` are:

- any OCaml flags field. See :doc:`concepts/ocaml-flags` for more details.

- ``(link_flags <flags>)`` specifies flags to OCaml when linking an executable.
  See :ref:`executables stanza <shared-exe-fields>`.

- ``(c_flags <flags>)`` and ``(cxx_flags <flags>)`` specify compilation flags
  for C and C++ stubs, respectively. See `library`_ for more details.

- ``(env-vars (<var1> <val1>) .. (<varN> <valN>))`` will add the corresponding
  variables to the environment where the build commands are executed and are
  used by ``dune exec``.

- ``(menhir_flags <flags>))`` specifies flags for Menhir stanzas.

- ``(js_of_ocaml (flags <flags>)(build_runtime <flags>)(link_flags <flags>))``
  specifies ``js_of_ocaml`` flags. See `jsoo-field`_ for more details.

- ``(js_of_ocaml (compilation_mode <mode>))`` controls whether to use separate
  compilation or not where ``<mode>`` is either ``whole_program`` or
  ``separate``.

- ``(js_of_ocaml (runtest_alias <alias-name>))`` specifies the alias under which
  :ref:`inline_tests` and tests (`tests-stanza`_) run for the `js` mode.

- ``(binaries <binaries>)``, where ``<binaries>`` is a list of entries of the
  form ``(<filepath> as <name>)``. ``(<filepath> as <name>)`` makes the binary
  ``<filepath>`` available in the command search as just ``<name>``. For
  instance, in a ``(run <name> ...)`` action, ``<name>`` will resolve to this
  file path. You can also write just the file path, in which case the name will
  be inferred from the basename of ``<filepath>`` by dropping the ``.exe``
  suffix, if it exists. For example, ``(binaries bin/foo.exe (bin/main.exe as
  bar))`` would add the commands ``foo`` and ``bar`` to the search path.

- ``(inline_tests <state>)``, where ``<state>`` is either ``enabled``,
  ``disabled``, or ``ignored``. This field has been available since Dune 1.11.
  It controls the variable's value ``%{inline_tests}``, which is read by the
  inline test framework. The default value is ``disabled`` for the ``release``
  profile and ``enabled`` otherwise.

- ``(odoc <fields>)`` allows passing options to ``odoc``. See
  :ref:`odoc-options` for more details.

- ``(coq (flags <flags>))`` allows passing options to Coq. See :ref:`coq-theory`
  for more details.

- ``(formatting <settings>)`` allows the user to set auto-formatting in the
  current directory subtree (see :ref:`formatting`).

- ``(bin_annot <bool>)`` allows the user to specify whether to generate `*.cmt`
  and `*.cmti` in the current directory subtree.
