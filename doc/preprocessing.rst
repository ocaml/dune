.. _preprocessing-specification:

***************************
Preprocessing specification
***************************

Dune accepts three kinds of preprocessing:

- ``no_preprocessing``, meaning that files are given as it to the compiler, this
  is the default
- ``(action <action>)`` to preprocess files using the given action
- ``(pps <ppx-rewriters-and-flags>)`` to preprocess files using the given list
  of ppx rewriters
- ``(staged_pps <ppx-rewriters-and-flags>)`` is similar to ``(pps ...)``
  but behave slightly differently and is needed for certain ppx rewriters
  (see below for details)
- ``future_syntax`` is a special value that brings some of the newer
  OCaml syntaxes to older compilers. See :ref:`Future syntax
  <future-syntax>` for more details

Dune normally assumes that the compilation pipeline is sequenced as
follow:

- code generation (including preprocessing)
- dependency analysis
- compilation

Dune uses this fact to optimize the pipeline and in particular share
the result of code generation and preprocessing between the dependency
analysis and compilation phases. However, some specific code
generators or preprocessors require feedback from the compilation
phase. As a result they must be applied in stages as follows:

- first stage of code generation
- dependency analysis
- second step of code generation in parallel with compilation

This is the case for ppx rewriters using the OCaml typer for
instance. When using such ppx rewriters, you must use ``staged_pps``
instead of ``pps`` in order to force Dune to use the second pipeline,
which is slower but necessary in this case.

.. _preprocessing-actions:

Preprocessing with actions
~~~~~~~~~~~~~~~~~~~~~~~~~~

``<action>`` uses the same DSL as described in the :ref:`user-actions`
section, and for the same reason given in that section, it will be
executed from the root of the current build context. It is expected to
be an action that reads the file given as only dependency named
``input-file`` and outputs the preprocessed file on its standard output.

More precisely, ``(preprocess (action <action>))`` acts as if
you had setup a rule for every file of the form:

   .. code:: scheme

       (rule
        (target file.pp.ml)
        (deps   file.ml)
        (action (with-stdout-to %{target}
                 (chdir %{workspace_root} <action>))))

The equivalent of a ``-pp <command>`` option passed to the OCaml compiler is
``(system "<command> %{input-file}")``.

Preprocessing with ppx rewriters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``<ppx-rewriters-and-flags>`` is expected to be a sequence where each
element is either a command line flag if starting with a ``-`` or the
name of a library.  If you want to pass command line flags that do not
start with a ``-``, you can separate library names from flags using
``--``. So for instance from the following ``preprocess`` field:

   .. code:: scheme

       (preprocess (pps ppx1 -foo ppx2 -- -bar 42))

The list of libraries will be ``ppx1`` and ``ppx2`` and the command line
arguments will be: ``-foo -bar 42``.

Libraries listed here should be libraries implementing an OCaml AST rewriter and
registering themselves using the `ocaml-migrate-parsetree.driver API
<https://github.com/let-def/ocaml-migrate-parsetree>`__.

Dune will build a single executable by linking all these libraries and their
dependencies. Note that it is important that all these libraries are linked with
``-linkall``. Dune automatically uses ``-linkall`` when the ``(kind ...)``
field is set to ``ppx_rewriter`` or ``ppx_deriver``.

Per module preprocessing specification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

By default a preprocessing specification will apply to all modules in the
library/set of executables. It is possible to select the preprocessing on a
module-by-module basis by using the following syntax:

 .. code:: scheme

    (preprocess (per_module
                 (<spec1> <module-list1>)
                 (<spec2> <module-list2>)
                 ...))

Where ``<spec1>``, ``<spec2>``, ... are preprocessing specifications
and ``<module-list1>``, ``<module-list2>``, ... are list of module
names.

For instance:

 .. code:: scheme

    (preprocess (per_module
                 (((action (run ./pp.sh X=1 %{input-file})) foo bar))
                 (((action (run ./pp.sh X=2 %{input-file})) baz))))

.. _future-syntax:

Future syntax
~~~~~~~~~~~~~

The ``future_syntax`` preprocessing specification is equivalent to
``no_preprocessing`` when using one of the most recent versions of the
compiler. When using an older one, it is a shim preprocessor that
backports some of the newer syntax elements. This allows you to use some of
the new OCaml features while keeping compatibility with older
compilers.

One example of supported syntax is the custom let-syntax that was
introduced in 4.08, allowing the user to define custom let operators.
