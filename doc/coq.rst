.. _coq:

***
Coq
***

.. TODO(diataxis)

   This looks like there are several components in there:

   - reference info for stanzas and variables
   - tutorials (the examples part)

.. contents:: Table of Contents
    :depth: 3

Introduction
------------

Dune can build Coq theories and plugins with additional support for extraction
and ``.mlg`` file preprocessing.

A *Coq theory* is a collection of ``.v`` files that define Coq modules whose
names share a common prefix. The module names reflect the directory hierarchy.

Coq theories may be defined using :ref:`coq.theory<coq-theory>` stanzas, or be
auto-detected by Dune by inspecting Coq's install directories.

A *Coq plugin* is an OCaml :doc:`/reference/dune/library` that Coq can
load dynamically at runtime. Plugins are typically linked with the Coq OCaml
API.

Since Coq 8.16, plugins need to be "public" libraries in Dune's terminology,
that is to say, they must declare a ``public_name`` field.

A *Coq project* is an informal term for a
:doc:`/reference/dune-project/index` containing a collection of Coq
theories and plugins.

The ``.v`` files of a theory need not be present as source files. They may also
be Dune targets of other rules.

To enable Coq support in a Dune project, specify the :ref:`Coq language
version<coq-lang>` in the :doc:`/reference/dune-project/index` file. For
example, adding

.. code:: dune

    (using coq 0.8)

to a :doc:`/reference/dune-project/index` file enables using the
``coq.theory`` stanza and other ``coq.*`` stanzas. See the :ref:`Dune Coq
language<coq-lang>` section for more details.

.. _coq-theory:

coq.theory
----------

The Coq theory stanza is very similar in form to the OCaml
:doc:`/reference/dune/library` stanza:

.. code:: dune

    (coq.theory
     (name <module_prefix>)
     (package <package>)
     (synopsis <text>)
     (modules <ordered_set_lang>)
     (plugins <ocaml_plugins>)
     (flags <coq_flags>)
     (modules_flags <flags_map>)
     (coqdoc_flags <coqdoc_flags>)
     (stdlib <stdlib_included>)
     (mode <coq_native_mode>)
     (theories <coq_theories>))

The stanza builds all the ``.v`` files in the given directory and its
subdirectories if the :ref:`include-subdirs <include-subdirs-coq>` stanza is
present.

For usage of this stanza, see the :ref:`examples`.

The semantics of the fields are:

- ``<module_prefix>`` is a dot-separated list of valid Coq module names and
  determines the module scope under which the theory is compiled (this
  corresponds to Coq's ``-R`` option).

  For example, if ``<module_prefix>`` is ``foo.Bar``, the theory modules are
  named ``foo.Bar.module1``, ``foo.Bar.module2``, etc. Note that modules in the
  same theory don't see the ``foo.Bar`` prefix in the same way that OCaml
  ``wrapped`` libraries do.

  For compatibility, :ref:`Coq lang 1.0<coq-lang-1.0>` installs a theory named
  ``foo.Bar`` under ``foo/Bar``. Also note that Coq supports composing a module
  path from different theories, thus you can name a theory ``foo.Bar`` and a
  second one ``foo.Baz``, and Dune composes these properly. See an example of
  :ref:`a multi-theory<example-multi-theory>` Coq project for this.

- The ``modules`` field allows one to constrain the set of modules included in
  the theory, similar to its OCaml counterpart. Modules are specified in Coq
  notation. That is to say, ``A/b.v`` is written ``A.b`` in this field.

- If the ``package`` field is present, Dune generates install rules for the
  ``.vo`` files of the theory. ``pkg_name`` must be a valid package name.

  Note that :ref:`Coq lang 1.0<coq-lang-1.0>` will use the Coq legacy install
  setup, where all packages share a common root namespace and install directory,
  ``lib/coq/user-contrib/<module_prefix>``, as is customary in the Make-based
  Coq package ecosystem.

  For compatibility, Dune also installs, under the ``user-contrib`` prefix, the
  ``.cmxs`` files that appear in ``<ocaml_plugins>``. This will be dropped in
  future versions.

- ``<coq_flags>`` are passed to ``coqc`` as command-line options. ``:standard``
  is taken from the value set in the ``(coq (flags <flags>))`` field in ``env``
  profile. See :doc:`/reference/dune/env` for more information.

- ``<flags_map>`` is a list of pairs of valid Coq module names and a
  list of ``<coq_flags>``. Note that if a module is present here, the
  ``:standard`` variable will be bound to the value of ``<coq_flags>``
  effective for the theory. This way it is possible to override the
  default flags for particular files of the theory, for example:

  .. code:: dune

    (coq.theory
      (name Foo)
      (modules_flags
        (bar (:standard \ -quiet))))


  It is more common to just use this field to *add* some particular
  flags, but that should be done using ``(:standard <flag1> <flag2>
  ...)`` as to propagate the default flags. (Appeared in :ref:`Coq
  lang 0.9<coq-lang>`)

- ``<coqdoc_flags>`` are extra user-configurable flags passed to ``coqdoc``. The
  default value for ``:standard`` is ``--toc``. The ``--html`` or ``--latex``
  flags are passed separately depending on which mode is target. See the section
  on :ref:`documentation using coqdoc<coqdoc>` for more information.

- ``<stdlib_included>`` can either be ``yes`` or ``no``, currently defaulting to
  ``yes``. When set to ``no``, Coq's standard library won't be visible from this
  theory, which means the ``Coq`` prefix won't be bound, and
  ``Coq.Init.Prelude`` won't be imported by default.

- If the ``plugins`` field is present, Dune will pass the corresponding flags to
  Coq so that ``coqdep`` and ``coqc`` can find the corresponding OCaml libraries
  declared in ``<ocaml_plugins>``. This allows a Coq theory to depend on OCaml
  plugins. Starting with ``(lang coq 0.6)``, ``<ocaml_plugins>`` must contain
  public library names.

- Your Coq theory can depend on other theories --- globally installed or defined
  in the current workspace --- by adding the theories names to the
  ``<coq_theories>`` field. Then, Dune will ensure that the depended theories
  are present and correctly registered with Coq.

  See :ref:`Locating Theories<locating-theories>` for more information on how
  Coq theories are located by Dune.

- If Coq has been configured with ``-native-compiler yes`` or ``ondemand``, Dune
  will always build the ``cmxs`` files together with the ``vo`` files. This only
  works on Coq versions after 8.13 in which the option was introduced.

  You may override this by specifying ``(mode native)`` or ``(mode vo)``.

  Before :ref:`Coq lang 0.7<coq-lang>`, the native mode had to be manually
  specified, and Coq did not use Coq's configuration

  Versions of Dune < 3.7.0 would disable native compilation if the ``dev``
  profile was selected.

- If the ``(mode vos)`` field is present, only Coq compiled interface files
  ``.vos`` will be produced for the theory. This is mainly useful in conjunction
  with ``dune coq top``, since this makes the compilation of dependencies much
  faster, at the cost of skipping proof checking. (Appeared in :ref:`Coq lang
  0.8<coq-lang>`).

Coq Dependencies
~~~~~~~~~~~~~~~~

When a Coq file ``a.v`` depends on another file ``b.v``, Dune is able to build
them in the correct order, even if they are in separate theories. Under the
hood, Dune asks coqdep how to resolve these dependencies, which is why it is
called once per theory.

.. _coqdoc:

Coq Documentation
~~~~~~~~~~~~~~~~~

Given a :ref:`coq-theory` stanza with ``name A``, Dune will produce two
*directory targets*, ``A.html/`` and ``A.tex/``. HTML or LaTeX documentation for
a Coq theory may then be built by running ``dune build A.html`` or ``dune build
A.tex``, respectively (if the :doc:`dune file </reference/dune/index>` for the
theory is the current directory).

There are also two aliases :doc:`/reference/aliases/doc` and ``@doc-latex``
that will respectively build the HTML or LaTeX documentation when called. These
will determine whether or not Dune passes a ``--html`` or ``--latex`` flag to
``coqdoc``.

Further flags can also be configured using the ``(coqdoc_flags)`` field in the
``coq.theory`` stanza. These will be passed to ``coqdoc`` and the default value
is ``:standard`` which is ``--toc``. Extra flags can therefore be passed by
writing ``(coqdoc_flags :standard --body-only)`` for example.

.. _include-subdirs-coq:

Recursive Qualification of Modules
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you add:

.. code:: dune

    (include_subdirs qualified)

to a :doc:`/reference/dune/index` file, Dune considers all the modules in
the directory and its subdirectories, adding a prefix to the module name in the
usual Coq style for subdirectories. For example, file ``A/b/C.v`` becomes the
module ``A.b.C``.

.. _locating-theories:

How Dune Locates and Builds theories
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Dune organises it's knowledge about Coq theories in 3 databases:

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

  Doing so is usually as simple as placing a Coq project within the scope of
  another. This process is termed *composition*. See the :ref:`interproject
  composition<example-interproject-theory>` example.

  Inter-project composition allows Dune to compute module dependencies using a
  fine granularity. In practice, this means that Dune will only build the parts
  of a depended theory that are needed by your project.

  Inter-project composition has been available since :ref:`Coq lang
  0.4<coq-lang>`.

- Installed theory database: If a theory cannot be found in the list of
  workspace-public theories, Dune will try to locate the theory in the list of
  installed locations Coq knows about.

  This list is built using the output of ``coqc --config`` in order  to infer
  the ``COQLIB`` and ``COQPATH`` environment variables. Each path in ``COQPATH``
  and ``COQLIB/user-contrib`` is used to build the database of installed
  theories.

  Note that, for backwards compatibility purposes, installed theories do not
  have to be installed or built using Dune. Dune tries to infer the name of the
  theory from the installed layout. This is ambiguous in the sense that a
  file-system layout of `a/b` will provide theory names ``a`` and ``a.b``.

  Resolving this ambiguity in a backwards-compatible way is not possible, but
  future versions of Dune Coq support will provide a way to improve this.

  Coq's standard library gets a special status in Dune. The location at
  ``COQLIB/theories`` will be assigned a entry with the theory name ``Coq``, and
  added to the dependency list implicitly. This can be disabled with the
  ``(stdlib no)`` field in the ``coq.theory`` stanza.

  The ``Coq`` prefix can then be used to depend on Coq's stdlib in a regular,
  qualified way. We recommend setting ``(stdlib no)`` and adding ``(theories
  Coq)`` explicitly.

  Composition with installed theories has been available since :ref:`Coq lang
  0.8<coq-lang>`.

The databases above are used to locate a theory dependencies. Note that Dune has
a complete global view of every file involved in the compilation of your theory
and will therefore rebuild if any changes are detected.

.. _public-private-theory:

Public and Private Theories
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A *public theory* is a :ref:`coq-theory` stanza that is visible outside the
scope of a :doc:`/reference/dune-project/index` file.

A *private theory* is a :ref:`coq-theory` stanza that is limited to the scope
of the :doc:`/reference/dune-project/index` file it is in.

A private theory may depend on both private and public theories; however, a
public theory may only depend on other public theories.

By default, all :ref:`coq-theory` stanzas are considered private by Dune. In
order to make a private theory into a public theory, the ``(package )`` field
must be specified.

.. code:: dune

  (coq.theory
   (name private_theory))

  (coq.theory
   (name private_theory)
   (package coq-public-theory))

Limitations
~~~~~~~~~~~

- ``.v`` files always depend on the native OCaml version of the Coq binary and
  its plugins, unless the natively compiled versions are missing.

.. _limitation-mlpack:

- A ``foo.mlpack`` file must the present in directories of locally defined
  plugins for things to work. ``coqdep``, which is used internally by Dune, will
  recognize a plugin by looking at the existence of an ``.mlpack`` file, as it
  cannot access (for now) Dune's library database. This is a limitation of
  ``coqdep``. See the :ref:`example plugin<example plugin>` or the `this
  template <https://github.com/ejgallego/coq-plugin-template>`_.

  This limitation will be lifted soon, as newer versions of ``coqdep`` can use
  findlib's database to check the existence of OCaml libraries.

.. _coq-lang:

Coq Language Version
~~~~~~~~~~~~~~~~~~~~

The Coq lang can be modified by adding the following to a
:doc:`/reference/dune-project/index` file:

.. code:: dune

    (using coq 0.8)

The supported Coq language versions (not the version of Coq) are:

- ``0.9``: Support for per-module flags with the ``(module_flags ...)``` field.
- ``0.8``: Support for composition with installed Coq theories;
  support for ``vos`` builds.

Deprecated experimental Coq language versions are:

- ``0.1``: Basic Coq theory support.
- ``0.2``: Support for the ``theories`` field and composition of theories in the
  same scope.
- ``0.3``: Support for ``(mode native)`` requires Coq >= 8.10 (and Dune >= 2.9
  for Coq >= 8.14).
- ``0.4``: Support for interproject composition of theories.
- ``0.5``: ``(libraries ...)`` field deprecated in favor of ``(plugins ...)``
  field.
- ``0.6``: Support for ``(stdlib no)``.
- ``0.7``: ``(mode )`` is automatically detected from the configuration of Coq
  and ``(mode native)`` is deprecated. The ``dev`` profile also no longer
  disables native compilation.

.. _coq-lang-1.0:

Coq Language Version 1.0
~~~~~~~~~~~~~~~~~~~~~~~~

Guarantees with respect to stability are not yet provided, but we
intend that the ``(0.8)`` version of the language becomes ``1.0``.
The ``1.0`` version of Coq lang will commit to a stable set of
functionality. All the features below are expected to reach ``1.0``
unchanged or minimally modified.

.. _coq-extraction:

coq.extraction
--------------

Coq may be instructed to *extract* OCaml sources as part of the compilation
process by using the ``coq.extraction`` stanza:

.. code:: dune

   (coq.extraction
    (prelude <name>)
    (extracted_modules <names>)
    <optional-fields>)

- ``(prelude <name>)`` refers to the Coq source that contains the extraction
  commands.

- ``(extracted_modules <names>)`` is an exhaustive list of OCaml modules
  extracted.

- ``<optional-fields>`` are ``flags``, ``stdlib``, ``theories``, and
  ``plugins``. All of these fields have the same meaning as in the
  ``coq.theory`` stanza.

The extracted sources can then be used in ``executable`` or ``library`` stanzas
as any other sources.

Note that the sources are extracted to the directory where the ``prelude`` file
lives. Thus the common placement for the ``OCaml`` stanzas is in the same
:doc:`/reference/dune/index` file.

**Warning**: using Coq's ``Cd`` command to work around problems with the output
directory is not allowed when using extraction from Dune. Moreover the ``Cd``
command has been deprecated in Coq 8.12.

.. _coq-pp:

coq.pp
------

Authors of Coq plugins often need to write ``.mlg`` files to extend the Coq
grammar. Such files are preprocessed with the ``coqpp`` binary. To help plugin
authors avoid writing boilerplate, we provide a ``(coq.pp ...)`` stanza:

.. code:: dune

    (coq.pp
     (modules <ordered_set_lang>))

This will run the ``coqpp`` binary on all the ``.mlg`` files in
``<ordered_set_lang>``.

.. _examples:

Examples of Coq Projects
------------------------

Here we list some examples of some basic Coq project setups in order.

.. _example-simple:

Simple Project
~~~~~~~~~~~~~~

Let us start with a simple project. First, make sure we have a
:doc:`/reference/dune-project/index` file with a :ref:`Coq
lang<coq-lang>` stanza present:

.. code:: dune

  (lang dune 3.16)
  (using coq 0.8)

Next we need a :doc:`/reference/dune/index` file with a :ref:`coq-theory`
stanza:

.. code:: dune

  (coq.theory
   (name myTheory))


Finally, we need a Coq ``.v`` file which we name ``A.v``:


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

.. _example-multi-theory:

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
  (coq.theory
   (name A))

  ; B/dune
  (coq.theory
   (name B)
   (theories A))

Notice the ``theories`` field in ``B`` allows one :ref:`coq-theory` to depend on
another. Another thing to note is the inclusion of the
:doc:`/reference/dune/include_subdirs` stanza. This allows our theory to
have :ref:`multiple subdirectories<include-subdirs-coq>`.

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

.. _example-interproject-theory:

Composing Projects
~~~~~~~~~~~~~~~~~~

To demonstrate the composition of Coq projects, we can take our previous two
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
  (* In Coq, partial prefixes for theory names are enough *)
  From A Require Import ab.
  From B Require Import b.

  (* Files from SimpleTheory *)
  From myTheory Require Import A.

We are referencing Coq modules from all three of our previously defined
theories.

Our :doc:`/reference/dune/index` file in ``CombinedWork`` looks like:

.. code:: dune

  (coq.theory
   (name Combined)
   (theories myTheory A B))

As you can see, there are dependencies on all the theories we mentioned.

All three of the theories we defined before were *private theories*. In order to
depend on them, we needed to make them *public theories*. See the section on
:ref:`public-private-theory`.

Composing With Installed Theories
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can also compose with theories that are installed. If we wanted to have a
theory that depends on the Coq theory ``mathcomp.ssreflect`` we can add the
following to our stanza:

.. code:: dune

  (coq.theory
   (name my_mathcomp_theory)
   (theories mathcomp.ssreflect))

Note that ``mathcomp`` on its own would also work, since there would be a
``matchcomp`` directory in ``user-contrib``, however it would not compose
locally with a ``coq.theory`` stanza with the ``mathcomp.ssreflect`` name (in
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
      ├── coqdoc.css
      ├── index.html
      └── toc.html

We may also want to build the LaTeX documentation of the theory ``B``. For this
we can call ``dune build B/B.tex/``. If we want to build all the HTML
documentation targets, we can use the :doc:`/reference/aliases/doc` alias as in
``dune build @doc``. If we want to build all the LaTeX documentation then we
use the ``@doc-latex`` alias instead.

.. _example plugin:

Coq Plugin Project
~~~~~~~~~~~~~~~~~~

Let us build a simple Coq plugin to demonstrate how Dune can handle this setup.

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
the plugin to sit in, otherwise Coq will not be able to find it.

.. code:: dune

  (lang dune 3.16)
  (using coq 0.8)

  (package
   (name my-coq-plugin)
   (synopsis "My Coq Plugin")
   (depends coq-core))

Now we have two directories, ``src/`` and ``theories/`` each with their own
:doc:`/reference/dune/index` file. Let us begin with the plugin
:doc:`/reference/dune/index` file:

.. code:: dune

  (library
   (name my_plugin)
   (public_name my-coq-plugin.plugin)
   (synopsis "My Coq Plugin")
   (flags :standard -rectypes -w -27)
   (libraries coq-core.vernac))

  (coq.pp
   (modules syntax))

Here we define a library using the :doc:`/reference/dune/library` stanza.
Importantly, we declared which external libraries we rely on and gave the
library a ``public_name``, as starting with Coq 8.16, Coq will identify plugins
using their corresponding findlib public name.

The :ref:`coq-pp` stanza allows ``src/syntax.mlg`` to be preprocessed, which for
reference looks like:

.. code:: ocaml

  DECLARE PLUGIN "my-coq-plugin.plugin"

  VERNAC COMMAND EXTEND CallToC CLASSIFIED AS QUERY
  | [ "Hello" ] -> { Feedback.msg_notice Pp.(str Hello_world.hello_world) }
  END

Together with ``hello_world.ml``:

.. code:: ocaml

  let hello_world = "hello world!"

They make up the plugin. There is one more important ingredient here and that is
the ``my_plugin.mlpack`` file, needed to signal ``coqdep`` the existence of
``my_plugin`` in this directory. An empty file suffices. See :ref:`this note on
.mlpack files<limitation-mlpack>`.

The file for ``theories/`` is a standard :ref:`coq-theory` stanza with an
included ``libraries`` field allowing Dune to see ``my-coq-plugin.plugin`` as a
dependency.

.. code:: dune

  (coq.theory
   (name MyPlugin)
   (package my-coq-plugin)
   (plugins my-coq-plugin.plugin))

Finally, our .v file will look something like this:

.. code:: coq

  (* For Coq < 8.16 *)
  Declare ML Module "my_plugin".

  (* For Coq = 8.16 *)
  Declare ML Module "my_plugin:my-coq-plugin.plugin".

  (* At some point Coq 8.17 or 8.18 will transition to the syntax below, check Coq's manual *)
  Declare ML Module "my-coq-plugin.plugin".

  Hello.

Running ``dune build`` will build everything correctly.

.. _running-coq-top:

Running a Coq Toplevel
----------------------

Dune supports running a Coq toplevel binary such as ``coqtop``, which is
typically used by editors such as CoqIDE or Proof General to interact with Coq.

The following command:

.. code:: console

   $ dune coq top <file> -- <args>

runs a Coq toplevel (``coqtop`` by default) on the given Coq file ``<file>``,
after having recompiled its dependencies as necessary. The given arguments
``<args>`` are forwarded to the invoked command. For example, this can be used
to pass a ``-emacs`` flag to ``coqtop``.

A different toplevel can be chosen with ``dune coq top --toplevel CMD <file>``.
Note that using ``--toplevel echo`` is one way to observe what options are
actually passed to the toplevel. These options are computed based on the options
that would be passed to the Coq compiler if it was invoked on the Coq file
``<file>``.

In certain situations, it is desirable to not rebuild dependencies for a ``.v``
files but still pass the correct flags to the toplevel. For this reason, a
``--no-build`` flag can be passed to ``dune coq top`` which will skip any
building of dependencies.

Limitations
~~~~~~~~~~~

* Only files that are part of a stanza can be loaded in a Coq toplevel.
* When a file is created, it must be written to the file system before the Coq
  toplevel is started.
* When new dependencies are added to a file (via a Coq ``Require`` vernacular
  command), it is in principle required to save the file and restart to Coq
  toplevel process.

.. _coq-variables:

Coq-Specific Variables
----------------------

There are some special variables that can be used to access data about the Coq
configuration. These are:

- ``%{coq:version}`` the version of Coq.
- ``%{coq:version.major}`` the major version of Coq (e.g., ``8.15.2`` gives
  ``8``).
- ``%{coq:version.minor}`` the minor version of Coq (e.g., ``8.15.2`` gives
  ``15``).
- ``%{coq:version.suffix}`` the suffix version of Coq (e.g., ``8.15.2`` gives
  ``.2`` and ``8.15+rc1`` gives ``+rc1``).
- ``%{coq:ocaml-version}`` the version of OCaml used to compile Coq.
- ``%{coq:coqlib}`` the output of ``COQLIB`` from ``coqc -config``.
- ``%{coq:coq_native_compiler_default}`` the output of
  ``COQ_NATIVE_COMPILER_DEFAULT`` from ``coqc -config``.

See :doc:`concepts/variables` for more information on variables supported by
Dune.


.. _coq-env:

Coq Environment Fields
----------------------

The :doc:`/reference/dune/env` stanza has a ``(coq <coq_fields>)`` field
with the following values for ``<coq_fields>``:

- ``(flags <flags>)``: The default flags passed to ``coqc``. The default value
  is ``-q``. Values set here become the ``:standard`` value in the
  ``(coq.theory (flags <flags>))`` field. 
- ``(coqdoc_flags <flags>)``: The default flags passed to ``coqdoc``. The default
  value is ``--toc``. Values set here become the ``:standard`` value in the
  ``(coq.theory (coqdoc_flags <flags>))`` field.
