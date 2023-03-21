************
Other Topics
************

This section describes some details of Dune for advanced users.

Classical PPX
=============

*Classical PPX* refers to running PPX using the ```-ppx`` compiler option, which is
composed using Findlib. Even though this is useful to run some (usually old)
PPXs that don't support drivers, Dune doesn't support preprocessing with
PPX this way. However, a workaround exists using the `ppxfind
<https://github.com/kandu/ppxfind>`_ tool.

Profiling Dune
==============

If ``--trace-file FILE`` is passed, Dune will write detailed data about internal
operations, such as the timing of commands that Dune runs.

The format is compatible with `Catapult trace-viewer`_. In particular, these
files can be loaded into Chromium's ``chrome://tracing``. Note that the exact
format is subject to change between versions.

.. _Catapult trace-viewer: https://github.com/catapult-project/catapult/blob/master/tracing/README.md

.. _package-version:

Package Version
===============

Dune determines a package's version by looking at the ``version``
field in the :ref:`package stanza <package>`. If the version field isn't 
set, it looks at the toplevel ``version`` field in the
``dune-project`` field. If neither are set, Dune assumes that we are in
development mode and reads the version from the VCS if any. The way it
obtains the version from the VCS in described in :ref:`the build-info
section <build-info>`.

When installing the files of a package on the system, Dune
automatically inserts the package version into various metadata files
such as ``META`` and ``dune-package`` files.

.. _ocaml-syntax:

OCaml Syntax
============

If a ``dune`` file starts with ``(* -*- tuareg -*- *)``, then it is
interpreted as an OCaml script that generates the ``dune`` file as described
in the rest of this section. The code in the script will have access to a
`Jbuild_plugin
<https://github.com/ocaml/dune/blob/master/plugin/jbuild_plugin.mli>`__
module containing details about the build context it's executed in.

The OCaml syntax gives you an escape hatch for when the S-expression
syntax is not enough. It isn't clear whether the OCaml syntax will be
supported in the long term, as it doesn't work well with incremental
builds. It is possible that it will be replaced by just an ``include``
stanza where one can include a generated file.

Consequently **you must not** build complex systems based on it.

.. _variables-for-artifacts:

Variables for Artifacts
-----------------------

For specific situations where one needs to refer to individual compilation
artifacts, special variables (see :doc:`concepts/variables`) are provided, so
the user doesn't need to be aware of the particular naming conventions or
directory layout implemented by Dune.

These variables can appear wherever a :doc:`concepts/dependency-spec` is
expected and also inside :doc:`concepts/actions`. When used inside
:doc:`concepts/actions`, they implicitly declare a dependency on the
corresponding artifact.

The variables have the form ``%{<ext>:<path>}``, where ``<path>`` is
interpreted relative to the current directory:

- ``cmo:<path>``, ``cmx:<path>``, and ``cmi:<path>`` expand to the corresponding
  artifact's path for the module specified by ``<path>``. The basename of
  ``<path>`` should be the name of a module as specified in a ``(modules)``
  field.

- ``cma:<path>`` and ``cmxa:<path>`` expands to the corresponding 
  artifact's path for the library specified by ``<path>``. The basename of ``<path>``
  should be the name of the library as specified in the ``(name)`` field of a
  ``library`` stanza (*not* its public name).

In each case, the expansion of the variable is a path pointing inside the build
context (i.e., ``_build/<context>``).

Building an Ad Hoc ``.cmxs``
----------------------------

In the model exposed by Dune, a ``.cmxs`` target is created for each
library. However, the ``.cmxs`` format itself is more flexible and is
capable to containing arbitrary ``.cmxa`` and ``.cmx`` files.

For the specific cases where this extra flexibility is needed, one can use
:ref:`variables-for-artifacts` to write explicit rules to build ``.cmxs`` files
not associated to any library.

Below is an example where we build ``my.cmxs`` containing ``foo.cmxa`` and
``d.cmx``. Note how we use a :ref:`library` stanza to set up the compilation of
``d.cmx``.

.. code:: dune

    (library
     (name foo)
     (modules a b c))

    (library
     (name dummy)
     (modules d))

    (rule
     (targets my.cmxs)
     (action (run %{ocamlopt} -shared -o %{targets} %{cmxa:foo} %{cmx:d})))
