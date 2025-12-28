.. _rocq:

******************************
The Rocq Prover Build Language
******************************

.. TODO(diataxis)

   This looks like there are several components in there:

   - reference info for stanzas and variables
   - tutorials (the examples part)

Introduction
------------

Dune can build Rocq theories and plugins with additional support for extraction
and ``.mlg`` file preprocessing.

A *Rocq theory* is a collection of ``.v`` files that define Rocq modules whose
names share a common prefix. The module names reflect the directory hierarchy.

Rocq theories may be defined using :ref:`rocq.theory<rocq-theory>` stanzas, or be
auto-detected by Dune by inspecting Rocq's install directories.

A *Rocq plugin* is an OCaml :doc:`/reference/dune/library` that Rocq can
load dynamically at runtime. Plugins are typically linked with the Rocq OCaml
API.

Rocq plugins need to be "public" libraries in Dune's terminology, that
is to say, they must declare a ``public_name`` field; this is
necessary as they are located using ``ocamlfind``.

A *Rocq project* is an informal term for a
:doc:`/reference/dune-project/index` containing a collection of Rocq
theories and plugins.

The ``.v`` files of a theory need not be present as source files. They may also
be Dune targets of other rules.

To enable Rocq support in a Dune project, specify the :ref:`Rocq language
version<rocq-lang>` in the :doc:`/reference/dune-project/index` file. For
example, adding

.. code:: dune

    (using rocq 0.11)

to a :doc:`/reference/dune-project/index` file enables using the
``rocq.theory`` stanza and other ``rocq.*`` stanzas. See the :ref:`Dune Rocq
language<rocq-lang>` section for more details.

.. _rocq-theory:

rocq.theory
-----------

The Rocq theory stanza is very similar in form to the OCaml
:doc:`/reference/dune/library` stanza:

.. code:: dune

    (rocq.theory
     (name <module_prefix>)
     (package <package>)
     (synopsis <text>)
     (modules <ordered_set_lang>)
     (plugins <ocaml_plugins>)
     (flags <rocq_flags>)
     (modules_flags <flags_map>)
     (generate_project_file)
     (rocqdep_flags <rocqdep_flags>)
     (rocqdoc_flags <rocqdoc_flags>)
     (rocqdoc_header <rocqdoc_header>)
     (rocqdoc_footer <rocqdoc_footer>)
     (stdlib <stdlib_included>)
     (mode <rocq_build_mode>)
     (theories <rocq_theories>))

The stanza builds all the ``.v`` files in the given directory and its
subdirectories if the :ref:`include-subdirs <include-subdirs-rocq>` stanza is
present.

For usage of this stanza, see the :ref:`rocq-examples`.

The semantics of the fields are:

- ``<module_prefix>`` is a dot-separated list of valid Rocq module names and
  determines the module scope under which the theory is compiled (this
  corresponds to Rocq's ``-R`` option).

  For example, if ``<module_prefix>`` is ``foo.Bar``, the theory modules are
  named ``foo.Bar.module1``, ``foo.Bar.module2``, etc. Note that modules in the
  same theory don't see the ``foo.Bar`` prefix in the same way that OCaml
  ``wrapped`` libraries do.

  For compatibility, :ref:`Rocq lang 1.0<rocq-lang-1.0>` installs a theory named
  ``foo.Bar`` under ``foo/Bar``. Also note that Rocq supports composing a module
  path from different theories, thus you can name a theory ``foo.Bar`` and a
  second one ``foo.Baz``, and Dune composes these properly. See an example of
  :ref:`a multi-theory<rocq-example-multi-theory>` Rocq project for this.

- The ``modules`` field allows one to constrain the set of modules included in
  the theory, similar to its OCaml counterpart. Modules are specified in Rocq
  notation. That is to say, ``A/b.v`` is written ``A.b`` in this field.

- If the ``package`` field is present, Dune generates install rules for the
  ``.vo`` files of the theory. ``pkg_name`` must be a valid package name.

  Note that :ref:`Rocq lang 1.0<rocq-lang-1.0>` uses the Rocq install
  setup, where all packages share a common root namespace and install
  directory, ``lib/rocq/user-contrib/<module_prefix>``, as is
  customary in the Make-based Rocq package ecosystem.

  For compatibility, Dune also installs, under the ``user-contrib`` prefix, the
  ``.cmxs`` files that appear in ``<ocaml_plugins>``. This will be dropped in
  future versions.

- ``<rocq_flags>`` are passed to ``rocqc`` as command-line options. ``:standard``
  is taken from the value set in the ``(rocq (flags <flags>))`` field in ``env``
  profile. See :doc:`/reference/dune/env` for more information.

- ``<flags_map>`` is a list of pairs of valid Rocq module names and a
  list of ``<rocq_flags>``. Note that if a module is present here, the
  ``:standard`` variable will be bound to the value of ``<rocq_flags>``
  effective for the theory. This way it is possible to override the
  default flags for particular files of the theory, for example:

  .. code:: dune

    (rocq.theory
      (name Foo)
      (modules_flags
        (bar (:standard \ -quiet))))


  It is more common to just use this field to *add* some particular
  flags, but that should be done using ``(:standard <flag1> <flag2>
  ...)`` as to propagate the default flags.

- ``<rocqdep_flags>`` are extra user-configurable flags passed to ``rocqdep``. The
  default value for ``:standard`` is empty. This field exists for transient
  use-cases, in particular disabling ``rocqdep`` warnings, but it should not be
  used in normal operations.

- ``<rocqdoc_flags>`` are extra user-configurable flags passed to ``rocqdoc``. The
  default value for ``:standard`` is ``--toc``. The ``--html`` or ``--latex``
  flags are passed separately depending on which mode is target. See the section
  on :ref:`documentation using rocqdoc<rocqdoc>` for more information.

- ``<rocqdoc_header>`` is a file passed to ``rocqdoc`` using the ``--with-header``
  option, to configure a custom HTML header for the generated HTML pages.

- ``<rocqdoc_footer>`` is a file passed to ``rocqdoc`` using the ``--with-footer``
  option, to configure a custom HTML footer for the generated HTML pages.

- ``<stdlib_included>`` can either be ``yes`` or ``no``, currently defaulting to
  ``yes``. When set to ``no``, Rocq's standard library won't be visible from this
  theory, which means the ``Rocq`` prefix won't be bound, and
  ``Rocq.Init.Prelude`` won't be imported by default.

- If the ``plugins`` field is present, Dune will pass the corresponding flags to
  Rocq so that ``rocqdep`` and ``rocqc`` can find the corresponding OCaml libraries
  declared in ``<ocaml_plugins>``. This allows a Rocq theory to depend on OCaml
  plugins. The field must contain a public library name.

- Your Rocq theory can depend on other theories --- globally installed or defined
  in the current workspace --- by adding the theories names to the
  ``<rocq_theories>`` field. Then, Dune will ensure that the depended theories
  are present and correctly registered with Rocq.

  See :ref:`Locating Theories<rocq-locating-theories>` for more information on how
  Rocq theories are located by Dune.

- The ``<rocq_build_mode>`` field does control what Rocq objects are
  built. Allowed values are ``vo`` or ``vos``.

  By default, Dune will build Rocq's ``.vo`` and ``.glob`` files.

  If Rocq was configured with ``-native-compiler yes``, Dune will
  also build the corresponding ``cmxs`` native files.

  You may disable the compilation of native objects in this case by
  specifying ``(mode vo)``, which can bring an important speedup in some cases.

  If ``(mode vos)`` is set, Dune will instead output ``.vos``
  interface files, which skip proof checking.

- If the ``(generate_project_file)`` is present, a ``_RocqProject`` file is
  generated in the Rocq theory's directory (it is promoted to the source tree).
  This file should be suitable for editor compatibility, and it provides an
  alternative to using ``dune rocq top``. It is however limited in two ways: it
  is incompatible with the ``(modules_flags ...)`` field, and it cannot be
  used for two Rocq theories declared in the same directory.

Rocq Dependencies
~~~~~~~~~~~~~~~~~

When a Rocq file ``a.v`` depends on another file ``b.v``, Dune is able to build
them in the correct order, even if they are in separate theories. Under the
hood, Dune asks rocqdep how to resolve these dependencies, which is why it is
called once per theory.

.. _rocqdoc:

Rocq Documentation
~~~~~~~~~~~~~~~~~~

Given a :ref:`rocq-theory` stanza with ``name A``, Dune will produce two
*directory targets*, ``A.html/`` and ``A.tex/``. HTML or LaTeX documentation for
a Rocq theory may then be built by running ``dune build A.html`` or ``dune build
A.tex``, respectively (if the :doc:`dune file </reference/dune/index>` for the
theory is the current directory).

There are also two aliases :doc:`/reference/aliases/doc` and ``@doc-latex``
that will respectively build the HTML or LaTeX documentation when called. These
will determine whether or not Dune passes a ``--html`` or ``--latex`` flag to
``rocqdoc``.

Further flags can also be configured using the ``(rocqdoc_flags)`` field in the
``rocq.theory`` stanza. These will be passed to ``rocqdoc`` and the default value
is ``:standard`` which is ``--toc``. Extra flags can therefore be passed by
writing ``(rocqdoc_flags :standard --body-only)`` for example.

When building the HTML documentation, flags ``(rocqdoc_header)`` and
``(rocqdoc_footer)`` can also be used to configure a custom HTML header or
footer respectively.

.. _include-subdirs-rocq:

Recursive Qualification of Modules
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you add:

.. code:: dune

    (include_subdirs qualified)

to a :doc:`/reference/dune/index` file, Dune considers all the modules in
the directory and its subdirectories, adding a prefix to the module name in the
usual Rocq style for subdirectories. For example, file ``A/b/C.v`` becomes the
module ``A.b.C``.

.. _rocq-locating-theories:

How Dune Locates and Builds theories
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Dune organises it's knowledge about Rocq theories in 3 databases:

- Scope database: A Dune *scope* is a part of the project sharing a single
  common ``dune-project`` file. In a single scope, any theory in the database
  can depend on any other theory in that database as long as their visibilities
  are compatible. A public theory for example cannot depend on a private
  theory.

- Public theory database: The set of all scopes that Dune knows about is termed
  a *workspace*. Only public theories coming from scopes are added to the
  database of all public theories in the current workspace.

  The public theory database allows theories to depend on theories that are in
  a different scope. Thus, you can depend on theories belonging to another
  :doc:`/reference/dune-project/index` as long as they share a common
  scope under another :doc:`/reference/dune-project/index` file or a
  :doc:`/reference/dune-workspace/index` file.

  Doing so is usually as simple as placing a Rocq project within the scope of
  another. This process is termed *composition*. See the :ref:`interproject
  composition<rocq-example-interproject-theory>` example.

  Inter-project composition allows Dune to compute module dependencies using a
  fine granularity. In practice, this means that Dune will only build the parts
  of a depended theory that are needed by your project.

- Installed theory database: If a theory cannot be found in the list of
  workspace-public theories, Dune will try to locate the theory in the list of
  installed locations Rocq knows about.

  This list is built using the output of ``rocqc --config`` in order  to infer
  the ``ROCQLIB`` and ``ROCQPATH`` environment variables. Each path in ``ROCQPATH``
  and ``ROCQLIB/user-contrib`` is used to build the database of installed
  theories.

  Note that, for backwards compatibility purposes, installed theories do not
  have to be installed or built using Dune. Dune tries to infer the name of the
  theory from the installed layout. This is ambiguous in the sense that a
  file-system layout of `a/b` will provide theory names ``a`` and ``a.b``.

  Resolving this ambiguity in a backwards-compatible way is not possible, but
  future versions of Dune Rocq support will provide a way to improve this.

  Rocq's standard library gets a special status in Dune. The location at
  ``ROCQLIB/theories`` will be assigned a entry with the theory name ``Rocq``, and
  added to the dependency list implicitly. This can be disabled with the
  ``(stdlib no)`` field in the ``rocq.theory`` stanza.

  The ``Rocq`` prefix can then be used to depend on Rocq's stdlib in a regular,
  qualified way. We recommend setting ``(stdlib no)`` and adding ``(theories
  Rocq)`` explicitly.

The databases above are used to locate a theory dependencies. Note that Dune has
a complete global view of every file involved in the compilation of your theory
and will therefore rebuild if any changes are detected.

.. _rocq-public-private-theory:

Public and Private Theories
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A *public theory* is a :ref:`rocq-theory` stanza that is visible outside the
scope of a :doc:`/reference/dune-project/index` file.

A *private theory* is a :ref:`rocq-theory` stanza that is limited to the scope
of the :doc:`/reference/dune-project/index` file it is in.

A private theory may depend on both private and public theories; however, a
public theory may only depend on other public theories.

By default, all :ref:`rocq-theory` stanzas are considered private by Dune. In
order to make a private theory into a public theory, the ``(package )`` field
must be specified.

.. code:: dune

  (rocq.theory
   (name private_theory))

  (rocq.theory
   (name private_theory)
   (package rocq-public-theory))

Limitations
~~~~~~~~~~~

- ``.v`` files always depend on the native OCaml version of the Rocq binary and
  its plugins, unless the natively compiled versions are missing.

.. _limitation-mlpack:

- A ``foo.mlpack`` file must the present in directories of locally defined
  plugins for things to work. ``rocqdep``, which is used internally by Dune, will
  recognize a plugin by looking at the existence of an ``.mlpack`` file, as it
  cannot access (for now) Dune's library database. This is a limitation of
  ``rocqdep``. See the :ref:`example plugin<rocq example plugin>` or the `this
  template <https://github.com/ejgallego/rocq-plugin-template>`_.

  This limitation will be lifted soon, as newer versions of ``rocqdep`` can use
  findlib's database to check the existence of OCaml libraries.

.. _rocq-lang:

Rocq Language Version
~~~~~~~~~~~~~~~~~~~~~

The Rocq lang can be modified by adding the following to a
:doc:`/reference/dune-project/index` file:

.. code:: dune

    (using rocq 0.11)

The supported Rocq language versions (not the version of Rocq) are:

- ``0.11``: Support for the Rocq Prover; most important changes are:
  + all deprecated features in ``(lang coq 0.10)`` have been removed.
  + Dune won't install .cmxs files in user-contrib (along .vo files) anymore.
  + ``(mode native)`` is not allowed anymore. It is the default if Rocq was configured with native compute enabled.
  + ``COQPATH`` is not recognized anymore, use ``ROCQPATH``.

.. _rocq-lang-1.0:

Rocq Language Version 1.0
~~~~~~~~~~~~~~~~~~~~~~~~~

Guarantees with respect to stability are not yet provided, but we
intend that the ``(0.11)`` version of the language becomes ``1.0``.
The ``1.0`` version of Rocq lang will commit to a stable set of
functionality. All the features below are expected to reach ``1.0``
unchanged or minimally modified.

.. _rocq-extraction:

rocq.extraction
---------------

Rocq may be instructed to *extract* OCaml sources as part of the compilation
process by using the ``rocq.extraction`` stanza:

.. code:: dune

   (rocq.extraction
    (prelude <name>)
    (extracted_modules <names>)
    <optional-fields>)

- ``(prelude <name>)`` refers to the Rocq source that contains the extraction
  commands.

- ``(extracted_modules <names>)`` is an exhaustive list of OCaml modules
  extracted.

- ``<optional-fields>`` are ``flags``, ``stdlib``, ``theories``, and
  ``plugins``. All of these fields have the same meaning as in the
  ``rocq.theory`` stanza.

The extracted sources can then be used in ``executable`` or ``library`` stanzas
as any other sources.

Note that the sources are extracted to the directory where the ``prelude`` file
lives. Thus the common placement for the ``OCaml`` stanzas is in the same
:doc:`/reference/dune/index` file.

**Warning**: using Rocq's ``Cd`` command to work around problems with the output
directory is not allowed when using extraction from Dune. Moreover the ``Cd``
command has been deprecated in Coq 8.12.

.. _rocq-pp:

rocq.pp
-------

Authors of Rocq plugins often need to write ``.mlg`` files to extend the Rocq
grammar. Such files are preprocessed with the ``rocqpp`` binary. To help plugin
authors avoid writing boilerplate, we provide a ``(rocq.pp ...)`` stanza:

.. code:: dune

    (rocq.pp
     (modules <ordered_set_lang>))

This will run the ``rocqpp`` binary on all the ``.mlg`` files in
``<ordered_set_lang>``.

.. _rocq-examples:

Examples of Rocq Projects
-------------------------

Here we list some examples of some basic Rocq project setups in order.

.. _rocq-example-simple:

Simple Project
~~~~~~~~~~~~~~

Let us start with a simple project. First, make sure we have a
:doc:`/reference/dune-project/index` file with a :ref:`Rocq
lang<rocq-lang>` stanza present:

.. code:: dune

  (lang dune 3.22)
  (using rocq 0.11)

Next we need a :doc:`/reference/dune/index` file with a :ref:`rocq-theory`
stanza:

.. code:: dune

  (rocq.theory
   (name myTheory))


Finally, we need a Rocq ``.v`` file which we name ``A.v``:


.. code:: coq

  (** This is my def *)
  Definition mydef := nat.

Now we run ``dune build``. After this is complete, we get the following files:

.. code::

  .
  ├── A.v
  ├── _build
  │   ├── default
  │   │   ├── A.glob
  │   │   ├── A.v
  │   │   └── A.vo
  │   └── log
  ├── dune
  └── dune-project

.. _rocq-example-multi-theory:

Multi-Theory Project
~~~~~~~~~~~~~~~~~~~~

Here is an example of a more complicated setup:

.. code::

  .
  ├── A
  │   ├── AA
  │   │   └── aa.v
  │   ├── AB
  │   │   └── ab.v
  │   └── dune
  ├── B
  │   ├── b.v
  │   └── dune
  └── dune-project

Here are the :doc:`/reference/dune/index` files:

.. code:: dune

  ; A/dune
  (include_subdirs qualified)
  (rocq.theory
   (name A))

  ; B/dune
  (rocq.theory
   (name B)
   (theories A))

Notice the ``theories`` field in ``B`` allows one :ref:`rocq-theory` to depend on
another. Another thing to note is the inclusion of the
:doc:`/reference/dune/include_subdirs` stanza. This allows our theory to
have :ref:`multiple subdirectories<include-subdirs-rocq>`.

Here are the contents of the ``.v`` files:

.. code:: coq

  (* A/AA/aa.v is empty *)

  (* A/AB/ab.v *)
  Require Import AA.aa.

  (* B/b.v *)
  From A Require Import AB.ab.

This causes a dependency chain ``b.v -> ab.v -> aa.v``. Now we might be
interested in building theory ``B``, so all we have to do is run ``dune build
B``. Dune will automatically build the theory ``A`` since it is a dependency.

.. _rocq-example-interproject-theory:

Composing Projects
~~~~~~~~~~~~~~~~~~

To demonstrate the composition of Rocq projects, we can take our previous two
examples and put them in project which has a theory that depends on theories in
both projects.

.. code::

  .
  ├── CombinedWork
  │   ├── comb.v
  │   └── dune
  ├── DeeperTheory
  │   ├── A
  │   │   ├── AA
  │   │   │   └── aa.v
  │   │   ├── AB
  │   │   │   └── ab.v
  │   │   └── dune
  │   ├── B
  │   │   ├── b.v
  │   │   └── dune
  │   ├── Deep.opam
  │   └── dune-project
  ├── dune-project
  └── SimpleTheory
      ├── A.v
      ├── dune
      ├── dune-project
      └── Simple.opam

The file ``comb.v`` looks like:

.. code:: coq

  (* Files from DeeperTheory *)
  From A.AA Require Import aa.
  (* In Rocq, partial prefixes for theory names are enough *)
  From A Require Import ab.
  From B Require Import b.

  (* Files from SimpleTheory *)
  From myTheory Require Import A.

We are referencing Rocq modules from all three of our previously defined
theories.

Our :doc:`/reference/dune/index` file in ``CombinedWork`` looks like:

.. code:: dune

  (rocq.theory
   (name Combined)
   (theories myTheory A B))

As you can see, there are dependencies on all the theories we mentioned.

All three of the theories we defined before were *private theories*. In order to
depend on them, we needed to make them *public theories*. See the section on
:ref:`rocq-public-private-theory`.

Composing With Installed Theories
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can also compose with theories that are installed. If we wanted to have a
theory that depends on the Rocq theory ``mathcomp.ssreflect`` we can add the
following to our stanza:

.. code:: dune

  (rocq.theory
   (name my_mathcomp_theory)
   (theories mathcomp.ssreflect))

Note that ``mathcomp`` on its own would also work, since there would be a
``matchcomp`` directory in ``user-contrib``, however it would not compose
locally with a ``rocq.theory`` stanza with the ``mathcomp.ssreflect`` name (in
case one exists). So it is advisable to use the actual theory name. Dune is not
able to validate theory names that have been installed since they do not include
their Dune metadata.

Building Documentation
~~~~~~~~~~~~~~~~~~~~~~

Following from our last example, we might wish to build the HTML documentation
for ``A``. We simply do ``dune build A/A.html/``. This will produce the
following files:

.. code::

  A
  ├── AA
  │   ├── aa.glob
  │   ├── aa.v
  │   └── aa.vo
  ├── AB
  │   ├── ab.glob
  │   ├── ab.v
  │   └── ab.vo
  └── A.html
      ├── A.AA.aa.html
      ├── A.AB.ab.html
      ├── rocqdoc.css
      ├── index.html
      └── toc.html

We may also want to build the LaTeX documentation of the theory ``B``. For this
we can call ``dune build B/B.tex/``. If we want to build all the HTML
documentation targets, we can use the :doc:`/reference/aliases/doc` alias as in
``dune build @doc``. If we want to build all the LaTeX documentation then we
use the ``@doc-latex`` alias instead.

.. _rocq example plugin:

Rocq Plugin Project
~~~~~~~~~~~~~~~~~~~

Let us build a simple Rocq plugin to demonstrate how Dune can handle this setup.

.. code::

  .
  ├── dune-project
  ├── src
  │   ├── dune
  │   ├── hello_world.ml
  │   ├── my_plugin.mlpack
  │   └── syntax.mlg
  └── theories
      ├── dune
      └── UsingMyPlugin.v

Our :doc:`/reference/dune-project/index` will need to have a package for
the plugin to sit in, otherwise Rocq will not be able to find it.

.. code:: dune

  (lang dune 3.22)
  (using rocq 0.11)

  (package
   (name my-rocq-plugin)
   (synopsis "My Rocq Plugin")
   (depends rocq-core))

Now we have two directories, ``src/`` and ``theories/`` each with their own
:doc:`/reference/dune/index` file. Let us begin with the plugin
:doc:`/reference/dune/index` file:

.. code:: dune

  (library
   (name my_plugin)
   (public_name my-rocq-plugin.plugin)
   (synopsis "My Rocq Plugin")
   (flags :standard -rectypes -w -27)
   (libraries rocq-core.vernac))

  (rocq.pp
   (modules syntax))

Here we define a library using the :doc:`/reference/dune/library` stanza.
Importantly, we declared which external libraries we rely on and gave the
library a ``public_name``, which Rocq will use to identify the plugin.

The :ref:`rocq-pp` stanza allows ``src/syntax.mlg`` to be preprocessed, which for
reference looks like:

.. code:: ocaml

  DECLARE PLUGIN "my-rocq-plugin.plugin"

  VERNAC COMMAND EXTEND Hello CLASSIFIED AS QUERY
  | [ "Hello" ] -> { Feedback.msg_notice Pp.(str Hello_world.hello_world) }
  END

Together with ``hello_world.ml``:

.. code:: ocaml

  let hello_world = "hello world!"

They make up the plugin. There is one more important ingredient here and that is
the ``my_plugin.mlpack`` file, needed to signal ``rocqdep`` the existence of
``my_plugin`` in this directory. An empty file suffices. See :ref:`this note on
.mlpack files<limitation-mlpack>`.

The file for ``theories/`` is a standard :ref:`rocq-theory` stanza with an
included ``libraries`` field allowing Dune to see ``my-rocq-plugin.plugin`` as a
dependency.

.. code:: dune

  (rocq.theory
   (name MyPlugin)
   (package my-rocq-plugin)
   (plugins my-rocq-plugin.plugin))

Finally, our .v file will look something like this:

.. code:: coq

  Declare ML Module "my-rocq-plugin.plugin".

  Hello.

Running ``dune build`` will build everything correctly.

.. _running-rocq-top:

Running a Rocq Toplevel
-----------------------

Dune supports running a Rocq toplevel binary such as ``rocqtop``, which is
typically used by editors such as RocqIDE or Proof General to interact with Rocq.

The following command:

.. code:: console

   $ dune rocq top <file> -- <args>

runs a Rocq toplevel (``rocqtop`` by default) on the given Rocq file ``<file>``,
after having recompiled its dependencies as necessary. The given arguments
``<args>`` are forwarded to the invoked command. For example, this can be used
to pass a ``-emacs`` flag to ``rocqtop``.

A different toplevel can be chosen with ``dune rocq top --toplevel CMD <file>``.
Note that using ``--toplevel echo`` is one way to observe what options are
actually passed to the toplevel. These options are computed based on the options
that would be passed to the Rocq compiler if it was invoked on the Rocq file
``<file>``.

In certain situations, it is desirable to not rebuild dependencies for a ``.v``
files but still pass the correct flags to the toplevel. For this reason, a
``--no-build`` flag can be passed to ``dune rocq top`` which will skip any
building of dependencies.

Limitations
~~~~~~~~~~~

* Only files that are part of a stanza can be loaded in a Rocq toplevel.
* When a file is created, it must be written to the file system before the Rocq
  toplevel is started.
* When new dependencies are added to a file (via a Rocq ``Require`` vernacular
  command), it is in principle required to save the file and restart to Rocq
  toplevel process.

.. _rocq-variables:

Rocq-Specific Variables
-----------------------

There are some special variables that can be used to access data about the Rocq
configuration. These are:

- ``%{rocq:version}`` the version of Rocq.
- ``%{rocq:version.major}`` the major version of Rocq (e.g., ``9.1.2`` gives
  ``9``).
- ``%{rocq:version.minor}`` the minor version of Rocq (e.g., ``9.1.2`` gives
  ``1``).
- ``%{rocq:version.suffix}`` the suffix version of Rocq (e.g., ``9.1.2`` gives
  ``.2`` and ``9.2+rc1`` gives ``+rc1``).
- ``%{rocq:ocaml-version}`` the version of OCaml used to compile Rocq.
- ``%{rocq:rocqlib}`` the output of ``ROCQLIB`` from ``rocqc -config``.
- ``%{rocq:rocq_native_compiler_default}`` the output of
  ``ROCQ_NATIVE_COMPILER_DEFAULT`` from ``rocqc -config``.

See :doc:`concepts/variables` for more information on variables supported by
Dune.


.. _rocq-env:

Rocq Environment Fields
-----------------------

The :doc:`/reference/dune/env` stanza has a ``(rocq <rocq_fields>)`` field
with the following values for ``<rocq_fields>``:

- ``(flags <flags>)``: The default flags passed to ``rocqc``. The default value
  is ``-q``. Values set here become the ``:standard`` value in the
  ``(rocq.theory (flags <flags>))`` field.
- ``(rocqdep_flags <flags>)``: The default flags passed to ``rocqdep``. The default
  value is empty. Values set here become the ``:standard`` value in the
  ``(rocq.theory (rocqdep_flags <flags>))`` field. As noted in the documentation
  of the ``(rocq.theory (rocqdep_flags <flags>))`` field, changing the ``rocqdep``
  flags is discouraged.
- ``(rocqdoc_flags <flags>)``: The default flags passed to ``rocqdoc``. The default
  value is ``--toc``. Values set here become the ``:standard`` value in the
  ``(rocq.theory (rocqdoc_flags <flags>))`` field.
- ``(rocqdoc_header <file>)``: The default HTML header passed to ``rocqdoc`` via
  the ``--with-header`` flag. Values set here become the ``:standard`` value in the
  ``(rocq.theory (rocqdoc_header <file>))`` field.
- ``(rocqdoc_footer <file>)``: The default HTML footer passed to ``rocqdoc`` via
  the ``--with-footer`` flag. Values set here become the ``:standard`` value in the
  ``(rocq.theory (rocqdoc_footer <file>))`` field.
