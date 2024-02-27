.. highlight:: dune

#############################
 Preprocessing Specification
#############################

Some stanzas including ``(library)`` accept a ``(preprocessing)`` field.
The possible values for its argument are:

.. productionlist:: pp-spec : <pp-module> : (per-module <per-module>+) per-module : ( <pp-module> <module>+ ) pp-module : no_preprocessing : (action <action>) : (pps <ppx-rewriters-and-flags>) : (staged_pps <ppx-rewriters-and-flags>) : future_syntax

******************
 no_preprocessing
******************

When ``no_preprocessing`` is passed, files are given as-is to the
compiler. This is the default behavior.

.. _preprocessing-actions:

****************************
 Preprocessing With Actions
****************************

In ``(action <action>)``, ``<action>`` uses the same DSL as described in
:doc:`actions/index`, and for the same reason given in that section, it
will be executed from the root of the current build context. It's
expected to be an action that reads the file given as a dependency named
``input-file`` and outputs the preprocessed file on its standard output.

More precisely, ``(preprocess (action <action>))`` acts as if you had
set up a rule for every file of the form:

.. code::

   (rule
    (target file.pp.ml)
    (deps file.ml)
    (action
     (with-stdout-to %{target}
      (chdir %{workspace_root} <action>))))

The equivalent of a ``-pp <command>`` option passed to the OCaml
compiler is ``(system "<command> %{input-file}")``.

*********************
 Using PPX Rewriters
*********************

If ``(pps <ppx-rewriters-and-flags>)`` is used, the corresponding
rewriters are set up using the "fast pipeline" (using a separate
preprocessing step). If ``(staged_pps <ppx-rewriters-and-flags>)`` is
used, they are set up using the "classic pipeline" (using the ``-ppx``
command-line argument).

The distinction between these pipelines is explained in
:doc:`../explanation/preprocessing`.

PPX rewriters need to be compiled as a driver to be used by Dune. To run
PPXs that do not support this (usually old ones), it is possible to use
the ppxfind_ tool.

.. _ppxfind: https://github.com/kandu/ppxfind

****************************
 Arguments to PPX Rewriters
****************************

In ``(pps <ppx-rewriters-and-flags>)`` and ``(staged_pps
<ppx-rewriters-and-flags>)``, ``<ppx-rewriters-and-flags>`` is a
sequence where each element is either a command line flag if it starts
with a ``-`` or the name of a library.

If you want to pass command line flags that don't start with a ``-``,
you can separate library names from flags using ``--``. So for instance
from the following ``preprocess`` field:

.. code::

   (preprocess (pps ppx1 -foo ppx2 -- -bar 42))

The list of libraries will be ``ppx1`` and ``ppx2``, and the command
line arguments will be: ``-foo -bar 42``.

***************
 Future Syntax
***************

The ``future_syntax`` specification is a special value that brings some
of the newer OCaml syntaxes to older compilers.

It is equivalent to ``no_preprocessing`` when using one of the most
recent versions of the compiler. When using an older one, it is a shim
preprocessor that backports some of the newer syntax elements. This
allows you to use some of the new OCaml features while keeping
compatibility with older compilers.

One example of supported syntax is the custom ``let-syntax`` that was
introduced in 4.08, allowing the user to define custom ``let``
operators.

Note that this feature is implemented by the third-party
`ocaml-syntax-shims project
<https://github.com/ocaml-ppx/ocaml-syntax-shims>`_, so if you use this
feature, you must also declare a dependency on this package.

****************************************
 Per-Module Preprocessing Specification
****************************************

By default, a preprocessing specification applies to all modules in the
library/set of executables. It's possible to select the preprocessing on
a module-by-module basis by using the ``(per-module ...)`` syntax. For
instance:

.. code::

   (preprocess
    (per_module
     ((action (run ./pp.sh X=1 %{input-file})) foo bar)
     ((action (run ./pp.sh X=2 %{input-file})) baz)))

The modules ``Foo`` and ``Bar`` will be preprocessed with ``pp.sh X=1``,
and ``Baz`` will be preprocessed with ``pp.sh X=2``.

.. _preprocessor-deps:

***************************
 Preprocessor Dependencies
***************************

If your preprocessor needs extra dependencies, you should use the
``preprocessor_deps`` field available in the ``library``,
``executable``, and ``executables`` stanzas. It uses the
:doc:`../concepts/dependency-spec` to declare what the preprocessor
needs.
