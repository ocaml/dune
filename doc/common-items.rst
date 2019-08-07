************
Common items
************

.. _ordered-set-language:

Ordered set language
--------------------

A few fields takes as argument an ordered set and can be specified using a small
DSL.

This DSL is interpreted by dune into an ordered set of strings using the
following rules:

- ``:standard`` denotes the standard value of the field when it is absent
- an atom not starting with a ``:`` is a singleton containing only this atom
- a list of sets is the concatenation of its inner sets
- ``(<sets1> \ <sets2>)`` is the set composed of elements of ``<sets1>`` that do
  not appear in ``<sets2>``

In addition, some fields support the inclusion of an external file using the
syntax ``(:include <filename>)``. This is useful for instance when you need to
run a script to figure out some compilation flags. ``<filename>`` is expected to
contain a single S-expression and cannot contain ``(:include ...)`` forms.

Note that inside an ordered set, the first element of a list cannot be
an atom except if it starts with `-` or `:`. The reason for this is
that we are planning to add simple programmatic features in the
futures so that one may write:

.. code::

   (flags (if (>= %{ocaml_version} 4.06) ...))

This restriction will allow to add this feature without introducing a
breaking changes. If you want to write a list where the first element
doesn't start by `-`, you can simply quote it: ``("x" y z)``.

Most fields using the ordered set language also support `Variables expansion`_.
Variables are expanded after the set language is interpreted.

.. _blang:

Boolean Language
----------------

The boolean language allows the user to define simple boolean expressions that
dune can evaluate. Here's a semi formal specification of the language:

.. code::

   op := '=' | '<' | '>' | '<>' | '>=' | '<='

   expr := (and <expr>+)
         | (or <expr>+)
         | (<op> <template> <template>)
         | <template>

After an expression is evaluated, it must be exactly the string ``true`` or
``false`` to be considered as a boolean. Any other value will be treated as an
error.

Here's a simple example of a condition that expresses running on OSX and having
an flambda compiler with the help of variable expansion:

.. code:: scheme

   (and %{ocamlc-config:flambda} (= %{ocamlc-config:system} macosx))

.. _variables:

Variables expansion
-------------------

Some fields can contains variables of the form ``%{var}`` that are
expanded by dune.

Dune supports the following variables:

- ``project_root`` is the root of the current project. It is typically the root
  of your project and as long as you have a ``dune-project`` file there,
  ``project_root`` is independent of the workspace configuration
- ``workspace_root`` is the root of the current workspace. Note that
  the value of ``workspace_root`` is not constant and depends on
  whether your project is vendored or not
-  ``CC`` is the C compiler command line (list made of the compiler
   name followed by its flags) that was used to compile OCaml in the
   current build context
-  ``CXX`` is the C++ compiler command line being used in the
   current build context
-  ``ocaml_bin`` is the path where ``ocamlc`` lives
-  ``ocaml`` is the ``ocaml`` binary
-  ``ocamlc`` is the ``ocamlc`` binary
-  ``ocamlopt`` is the ``ocamlopt`` binary
-  ``ocaml_version`` is the version of the compiler used in the
   current build context
-  ``ocaml_where`` is the output of ``ocamlc -where``
-  ``arch_sixtyfour`` is ``true`` if using a compiler targeting a
   64 bit architecture and ``false`` otherwise
-  ``null`` is ``/dev/null`` on Unix or ``nul`` on Windows
-  ``ext_obj``, ``ext_asm``, ``ext_lib``, ``ext_dll`` and ``ext_exe``
   are the file extension used for various artifacts
- ``ocaml-config:v`` for every variable ``v`` in the output of
  ``ocamlc -config``. Note that dune processes the output
  of ``ocamlc -config`` in order to make it a bit more stable across
  versions, so the exact set of variables accessible this way might
  not be exactly the same as what you can see in the output of
  ``ocamlc -config``. In particular, variables added in new versions
  of OCaml needs to be registered in dune before they can be used
- ``profile`` the profile selected via ``--profile``
- ``context_name`` the name of the context (``default`` or defined in the
  workspace file)
- ``os_type`` is the type of the OS the build is targetting. This is
  the same as ``ocaml-config:os_type``
- ``architecture`` is the type of the architecture the build is targetting. This
  is the same as ``ocaml-config:architecture``
- ``model`` is the type of the cpu the build is targetting. This is
  the same as ``ocaml-config:model``
- ``system`` is the name of the OS the build is targetting. This is the same as
  ``ocaml-config:system``
- ``ignoring_promoted_rule`` is ``true`` if
  ``--ignore-promoted-rules`` was passed on the command line and
  ``false`` otherwise

In addition, ``(action ...)`` fields support the following special variables:

- ``target`` expands to the one target
- ``targets`` expands to the list of target
- ``deps`` expands to the list of dependencies
- ``^`` expands to the list of dependencies, separated by spaces
- ``dep:<path>`` expands to ``<path>`` (and adds ``<path>`` as a dependency of
  the action)
- ``exe:<path>`` is the same as ``<path>``, except when cross-compiling, in
  which case it will expand to ``<path>`` from the host build context
- ``bin:<program>`` expands to a path to ``program``. If ``program``
  is installed by a package in the workspace (see :ref:`install` stanzas),
  the locally built binary will be used, otherwise it will be searched
  in the ``PATH`` of the current build context. Note that ``(run
  %{bin:program} ...)`` and ``(run program ...)`` behave in the same
  way. ``%{bin:...}`` is only necessary when you are using ``(bash
  ...)`` or ``(system ...)``
- ``lib:<public-library-name>:<file>`` expands to a path to file ``<file>`` of
  library ``<public-library-name>``. If ``<public-library-name>`` is available
  in the current workspace, the local file will be used, otherwise the one from
  the installed world will be used
- ``libexec:<public-library-name>:<file>`` is the same as ``lib:...`` except
  when cross-compiling, in which case it will expand to the file from the host
  build context
- ``lib-available:<library-name>`` expands to ``true`` or ``false`` depending on
  whether the library is available or not. A library is available iff at least
  one of the following condition holds:

  -  it is part the installed worlds
  -  it is available locally and is not optional
  -  it is available locally and all its library dependencies are
     available

- ``version:<package>`` expands to the version of the given
  package. Note that this is only supported for packages that are
  being defined in the current scope
- ``read:<path>`` expands to the contents of the given file
- ``read-lines:<path>`` expands to the list of lines in the given
  file
- ``read-strings:<path>`` expands to the list of lines in the given
  file, unescaped using OCaml lexical convention

The ``%{<kind>:...}`` forms are what allows you to write custom rules that work
transparently whether things are installed or not.

Note that aliases are ignored by ``%{deps}``

The intent of this last form is to reliably read a list of strings
generated by an OCaml program via:

.. code:: ocaml

    List.iter (fun s -> print_string (String.escaped s)) l

#. Expansion of lists

Forms that expands to list of items, such as ``%{cc}``, ``%{deps}``,
``%{targets}`` or ``%{read-lines:...}``, are suitable to be used in, say,
``(run <prog> <arguments>)``.  For instance in:

.. code:: scheme

    (run foo %{deps})

if there are two dependencies ``a`` and ``b``, the produced command
will be equivalent to the shell command:

.. code:: shell

    $ foo "a" "b"

If you want the two dependencies to be passed as a single argument,
you have to quote the variable as in:

.. code:: scheme

    (run foo "%{deps}")

which is equivalent to the following shell command:

.. code:: shell

    $ foo "a b"

(the items of the list are concatenated with space).
Note that, since ``%{deps}`` is a list of items, the first one may be
used as a program name, for instance:

.. code:: scheme

    (rule
     (targets result.txt)
     (deps    foo.exe (glob_files *.txt))
     (action  (run %{deps})))

Here is another example:

.. code:: scheme

    (rule
     (target foo.exe)
     (deps   foo.c)
     (action (run %{cc} -o %{target} %{deps} -lfoolib)))


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

In all these cases, the argument supports `Variables expansion`_.

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

For all these fields, ``<flags>`` is specified in the `Ordered set language`_.
These fields all support ``(:include ...)`` forms.

The default value for ``(flags ...)`` is taken from the environment,
as a result it is recommended to write ``(flags ...)`` fields as
follows:

.. code:: scheme

    (flags (:standard <my options>))

.. _dune-jsoo:

js_of_ocaml
-----------

A :ref:`dune-jsoo-field` exists in executable and libraries stanzas that allows
one to customize options relevant to jsoo.

Sandboxing
----------

The user actions that run external commands (``run``, ``bash``, ``system``)
are opaque to dune, so dune has to rely on manual specification of dependencies
and targets. One problem with manual specification is that it's error-prone.
It's often hard to know in advance what files the command will read.
And knowing a correct set of dependencies is very important for build
reproducibility and incremental build correctness.

To help with this problem dune supports sandboxing.
An idealized view of sandboxing is that it runs the action in an environment
where it can't access anything except for its declared dependencies.

In practice we have to make compromises and have some trade-offs between
simplicity, information leakage, performance and portability.

The way sandboxing is currently implemented is that for each sandboxed action
we build a separate directory tree (sandbox directory) that mirrors the build
directory, filtering it to only contain the files that were declared as
dependencies. Then we run the action in that directory, and then we copy
the targets back to the build directory.

You can configure dune to use sandboxing modes ``symlink`` or ``copy``, which
determines how the individual files are populated (they will be symlinked or
copied into the sandbox directory).

This approach is very simple and portable, but that comes with
certain limitations:

- The actions in the sandbox can use absolute paths to refer to anywhere outside
  the sandbox. This means that only dependencies on relative paths in the build
  tree can be enforced/detected by sandboxing.
- The sandboxed actions still run with full permissions of dune itself so
  sandboxing is not a security feature. It won't prevent network access either.
- We don't erase the environment variables of the sandboxed
  commands. This is something we want to change.
- Performance impact is usually small, but it can get noticeable for
  fast actions with very large sets of dependencies.

Per-action sandboxing configuration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some actions may rely on sandboxing to work correctly.
For example an action may need the input directory to contain nothing
except the input files, or the action might create temporary files that
break other build actions.

Some other actions may refuse to work with sandboxing, for example
if they rely on absolute path to the build directory staying fixed,
or if they deliberately use some files without declaring dependencies
(this is usually a very bad idea, by the way).

Generally it's better to improve the action so it works with or without
sandboxing (especially with), but sometimes you just can't do that.

Things like this can be described using the "sandbox" field in the dependency
specification language (see :ref:`deps-field`).


Global sandboxing configuration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Dune always respects per-action sandboxing specification.
You can configure it globally to prefer a certain sandboxing mode if
the action allows it.

This is controlled by:

- ``dune --sandbox <...>`` cli flag (see ``man dune-build``)
- ``DUNE_SANDBOX`` environment (see ``man dune-build``)
- ``(sandboxing_preference ..)`` field in the dune config (see ``man dune-config``)

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

.. _ocaml-syntax:

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
