************
Common items
************

.. _library-dependencies:

Library dependencies
--------------------

Dependencies on libraries are specified using ``(libraries ...)`` fields in
``library`` and ``executables`` stanzas.

For libraries defined in the current scope, you can use either the real name or
the public name. For libraries that are part of the installed world, or for
libraries that are part of the current workspace but in another scope, you need
to use the public name. For instance: ``(libraries base re)``.

When resolving libraries, libraries that are part of the workspace are always
preferred to ones that are part of the installed world.

.. _alternative-deps:

Alternative dependencies
~~~~~~~~~~~~~~~~~~~~~~~~

In addition to direct dependencies you can specify alternative dependencies.
This is described in the :ref:`Alternative dependencies <alternative-deps>`
section

It is sometimes the case that one wants to not depend on a specific library, but
instead on whatever is already installed. For instance to use a different
backend depending on the target.

Dune allows this by using a ``(select ... from ...)`` form inside the list
of library dependencies.

Select forms are specified as follows:

.. code:: scheme

    (select <target-filename> from
     (<literals> -> <filename>)
     (<literals> -> <filename>)
     ...)

``<literals>`` are lists of literals, where each literal is one of:

- ``<library-name>``, which will evaluate to true if ``<library-name>`` is
  available, either in the workspace or in the installed world
- ``!<library-name>``, which will evaluate to true if ``<library-name>`` is not
  available in the workspace or in the installed world

When evaluating a select form, dune will create ``<target-filename>`` by
copying the file given by the first ``(<literals> -> <filename>)`` case where
all the literals evaluate to true. It is an error if none of the clauses are
selectable. You can add a fallback by adding a clause of the form ``(->
<file>)`` at the end of the list.

.. _preprocessing-specification:

Preprocessing specification
---------------------------

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

.. _deps-field:

Dependency specification
------------------------

Dependencies in ``dune`` files can be specified using one of the following:

.. _source_tree:

- ``(:name <dependencies>)`` will bind the the list of dependencies to the
  ``name`` variable. This variable will be available as ``%{name}`` in actions.
- ``(file <filename>)`` or simply ``<filename>``: depend on this file
- ``(alias <alias-name>)``: depend on the construction of this alias, for
  instance: ``(alias src/runtest)``
- ``(alias_rec <alias-name>)``: depend on the construction of this
  alias recursively in all children directories wherever it is
  defined. For instance: ``(alias_rec src/runtest)`` might depend on
  ``(alias src/runtest)``, ``(alias src/foo/bar/runtest)``, ...
- ``(glob_files <glob>)``: depend on all files matched by ``<glob>``, see the
  :ref:`glob <glob>` for details
- ``(source_tree <dir>)``: depend on all source files in the subtree with root
  ``<dir>``

- ``(universe)``: depend on everything in the universe. This is for
  cases where dependencies are too hard to specify. Note that dune
  will not be able to cache the result of actions that depend on the
  universe. In any case, this is only for dependencies in the
  installed world, you must still specify all dependencies that come
  from the workspace.
- ``(package <pkg>)`` depend on all files installed by ``<package>``, as well
  as on the transitive package dependencies of ``<package>``. This can be used
  to test a command against the files that will be installed
- ``(env_var <var>)``: depend on the value of the environment variable ``<var>``.
  If this variable becomes set, becomes unset, or changes value, the target
  will be rebuilt.
- ``(sandbox <config>)``: require a particular sandboxing configuration.
  Config can be one (or many) of:
  - ``always``: the action requires a clean environment.
  - ``none``: the action must run in the build directory.
  - ``preserve_file_kind``: the action needs the files it reads to look
  like normal files (so dune won't use symlinks for sandboxing)

In all these cases, the argument supports :ref:`variables`.

Named Dependencies
~~~~~~~~~~~~~~~~~~

dune allows a user to organize dependency lists by naming them. The user is
allowed to assign a group of dependencies a name that can later be referred to
in actions (like the ``%{deps}``, ``%{target}`` and ``%{targets}`` built in variables).

One instance where this is useful is for naming globs. Here's an
example of an imaginary bundle command:

.. code:: scheme

   (rule
    (target archive.tar)
    (deps
     index.html
     (:css (glob_files *.css))
     (:js foo.js bar.js)
     (:img (glob_files *.png) (glob_files *.jpg)))
    (action
     (run %{bin:bundle} index.html -css %{css} -js %{js} -img %{img} -o %{target})))

Note that such named dependency list can also include unnamed
dependencies (like ``index.html`` in the example above). Also, such
user defined names will shadow built in variables. So
``(:workspace_root x)`` will shadow the built in ``%{workspace_root}``
variable.

.. _glob:

Glob
~~~~

You can use globs to declare dependencies on a set of files. Note that globs
will match files that exist in the source tree as well as buildable targets, so
for instance you can depend on ``*.cmi``.

Currently dune only support globbing files in a single directory. And in
particular the glob is interpreted as follows:

- anything before the last ``/`` is taken as a literal path
- anything after the last ``/``, or everything if the glob contains no ``/``, is
  interpreted using the glob syntax

The glob syntax is interpreted as follows:

- ``\<char>`` matches exactly ``<char>``, even if it is a special character
  (``*``, ``?``, ...)
- ``*`` matches any sequence of characters, except if it comes first in which
  case it matches any character that is not ``.`` followed by anything
- ``**`` matches any character that is not ``.`` followed by anything, except if
  it comes first in which case it matches anything
- ``?`` matches any single character
- ``[<set>]`` matches any character that is part of ``<set>``
- ``[!<set>]`` matches any character that is not part of ``<set>``
- ``{<glob1>,<glob2>,...,<globn>}`` matches any string that is matched by one of
  ``<glob1>``, ``<glob2>``, ...

.. _ocaml-flags:

OCaml flags
-----------

In ``library``, ``executable``, ``executables`` and ``env`` stanzas,
you can specify OCaml compilation flags using the following fields:

- ``(flags <flags>)`` to specify flags passed to both ``ocamlc`` and
  ``ocamlopt``
- ``(ocamlc_flags <flags>)`` to specify flags passed to ``ocamlc`` only
- ``(ocamlopt_flags <flags>)`` to specify flags passed to ``ocamlopt`` only

For all these fields, ``<flags>`` is specified in the :ref:`ordered-set-language`.
These fields all support ``(:include ...)`` forms.

The default value for ``(flags ...)`` is taken from the environment,
as a result it is recommended to write ``(flags ...)`` fields as
follows:

.. code:: scheme

    (flags (:standard <my options>))

.. _locks:

Locks
-----

Given two rules that are independent, dune will assume that there
associated action can be run concurrently. Two rules are considered
independent if none of them depend on the other, either directly or
through a chain of dependencies. This basic assumption allows to
parallelize the build.

However, it is sometimes the case that two independent rules cannot be
executed concurrently. For instance this can happen for more
complicated tests. In order to prevent dune from running the
actions at the same time, you can specify that both actions take the
same lock:

.. code:: scheme

    (alias
     (name   runtest)
     (deps   foo)
     (locks  m)
     (action (run test.exe %{deps})))

    (alias
     (name   runtest)
     (deps   bar)
     (locks  m)
     (action (run test.exe %{deps})))

Dune will make sure that the executions of ``test.exe foo`` and
``test.exe bar`` are serialized.

Although they don't live in the filesystem, lock names are interpreted as file
names. So for instance ``(with-lock m ...)`` in ``src/dune`` and ``(with-lock
../src/m)`` in ``test/dune`` refer to the same lock.

Note also that locks are per build context. So if your workspace has two build
contexts setup, the same rule might still be executed concurrently between the
two build contexts. If you want a lock that is global to all build contexts,
simply use an absolute filename:

.. code:: scheme

    (alias
     (name   runtest)
     (deps   foo)
     (locks  /tcp-port/1042)
     (action (run test.exe %{deps})))

.. _diffing-and-promotion:

Diffing and promotion
---------------------

``(diff <file1> <file2>)`` is very similar to ``(run diff <file1>
<file2>)``. In particular it behaves in the same way:

- when ``<file1>`` and ``<file2>`` are equal, it doesn't nothing
- when they are not, the differences are shown and the action fails

However, it is different for the following reason:

- the exact command used to diff files can be configured via the
  ``--diff-command`` command line argument. Note that it is only
  called when the files are not byte equals

- by default, it will use ``patdiff`` if it is installed. ``patdiff``
  is a better diffing program. You can install it via opam with:

  .. code:: sh

     $ opam install patdiff

- on Windows, both ``(diff a b)`` and ``(diff? a b)`` normalize the end of
  lines before comparing the files

- since ``(diff a b)`` is a builtin action, dune knowns that ``a``
  and ``b`` are needed and so you don't need to specify them
  explicitly as dependencies

- you can use ``(diff? a b)`` after a command that might or might not
  produce ``b``. For cases where commands optionally produce a
  *corrected* file

- it allows promotion. See below

Note that ``(cmp a b)`` does no end of lines normalization and doesn't
print a diff when the files differ. ``cmp`` is meant to be used with
binary files.

Promotion
~~~~~~~~~

Whenever an action ``(diff <file1> <file2>)`` or ``(diff?  <file1>
<file2>)`` fails because the two files are different, dune allows
you to promote ``<file2>`` as ``<file1>`` if ``<file1>`` is a source
file and ``<file2>`` is a generated file.

More precisely, let's consider the following dune file:

.. code:: scheme

   (rule
    (with-stdout-to data.out (run ./test.exe)))

   (alias
    (name   runtest)
    (action (diff data.expected data.out)))

Where ``data.expected`` is a file committed in the source
repository. You can use the following workflow to update your test:

- update the code of your test
- run ``dune runtest``. The diff action will fail and a diff will
  be printed
- check the diff to make sure it is what you expect
- run ``dune promote``. This will copy the generated ``data.out``
  file to ``data.expected`` directly in the source tree

You can also use ``dune runtest --auto-promote`` which will
automatically do the promotion.

.. _ocaml-syntax:

OCaml syntax
============

If a ``dune`` file starts with ``(* -*- tuareg -*- *)``, then it is
interpreted as an OCaml script that generates the ``dune`` file as described
in the rest of this section. The code in the script will have access to a
`Jbuild_plugin
<https://github.com/ocaml/dune/blob/master/plugin/jbuild_plugin.mli>`__
module containing details about the build context it is executed in.

The OCaml syntax gives you an escape hatch for when the S-expression
syntax is not enough. It is not clear whether the OCaml syntax will be
supported in the long term as it doesn't work well with incremental
builds. It is possible that it will be replaced by just an ``include``
stanza where one can include a generated file.

Consequently **you must not** build complex systems based on it.
