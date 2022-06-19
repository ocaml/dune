.. _coq:

***
Coq
***

.. contents:: Table of Contents
    :depth: 3

Introduction
------------

Dune can build Coq theories and plugins with additional support for extraction
and ``.mlg`` file preprocessing.

A *Coq theory* is a collection of ``.v`` files that define Coq modules whose
names share a common prefix. The module names reflect the directory hierarchy.

A *Coq plugin* is an OCaml :ref:`library` that Coq can load dynamically at
runtime. Plugins are typically linked with the Coq OCaml API.

A *Coq project* is an informal term for a :ref:`dune-project` containing a
collection of Coq theories and plugins.

The ``.v`` files of a theory need not be present as source files. They may also
be Dune targets of other rules.

To enable Coq support in a Dune project, specify the :ref:`Coq language
version<coq-lang>` in the :ref:`dune-project` file. For example, adding

.. code:: scheme

    (using coq 0.4)

to a :ref:`dune-project` file enables using the ``coq.theory`` stanza and other
``coq.*`` stanzas. See the :ref:`Dune Coq language<coq-lang>` section for more
details.

.. _coq-theory:

coq.theory
----------

The Coq theory stanza is very similar in form to the OCaml :ref:`library`
stanza:

.. code:: scheme

    (coq.theory
     (name <module_prefix>)
     (package <package>)
     (synopsis <text>)
     (modules <ordered_set_lang>)
     (libraries <ocaml_libraries>)
     (flags <coq_flags>)
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

- If ``package`` is present, Dune generates install rules for the ``.vo`` files
  of the theory. ``pkg_name`` must be a valid package name.

  Note that :ref:`Coq lang 1.0<coq-lang-1.0>` will use the Coq legacy install
  setup, where all packages share a common root namespace and install directory,
  ``lib/coq/user-contrib/<module_prefix>``, as is customary in the Make-based
  Coq package ecosystem.

  For compatibility, Dune also installs, under the ``user-contrib`` prefix, the
  ``.cmxs`` files that appear in ``<ocaml_libraries>``.

- ``<coq_flags>`` are passed to ``coqc`` as command-line options. ``:standard``
  is taken from the value set in the ``(coq (flags <flags>))`` field in ``env``
  profile. See :ref:`dune-env` for more information.

- The path to the installed locations of the ``<ocaml_libraries>`` is passed to
  ``coqdep`` and ``coqc`` using Coq's ``-I`` flag. This allows a Coq theory to
  depend on an OCaml library.

- Your Coq theory can depend on other theories by specifying them in the
  ``<coq_theories>`` field. Dune then passes to Coq the corresponding flags for
  everything to compile correctly (this corresponds to the ``-Q`` flag for Coq).

  You may also depend on theories belonging to another :ref:`dune-project` as
  long as they share a common scope under another :ref:`dune-project` file or a
  :ref:`dune-workspace` file.

  Doing so can be as simple as placing a Coq project within the scope of
  another. This process is termed *composition*. See the :ref:`interproject
  composition<example-interproject-theory>` example. 
  
  Interproject composition allows for a fine granularity of dependencies. In
  practice, this means that Dune will only build the parts of a dependency that
  are needed. This means that building a project depending on another will not
  trigger a rebuild of the whole of the latter.
  
  Interproject composition has been available since :ref:`Coq lang
  0.4<coq-lang>`.

  As of today, Dune cannot depend on installed Coq theories. This restriction
  will be lifted in the future. Note that composition with the Coq standard
  library is supported, but in this case the ``Coq`` prefix has been made
  available in a qualified way, since :ref:`Coq lang 0.2<coq-lang>`.

  You may still use installed libraries in your Coq project, but there is
  currently no way for Dune to know about it.

- You can enable the production of Coq's native compiler object files by setting
  ``<coq_native_mode>`` to ``native``. This passes ``-native-compiler on`` to
  Coq and install the corresponding object files under ``.coq-native``, when in
  the ``release`` profile. The regular ``dev`` profile skips native compilation
  to make the build faster. This has been available since :ref:`Coq lang
  0.3<coq-lang>`.

  Please note: support for ``native_compute`` is **experimental** and requires a
  version of Coq later than 8.12.1. Furthermore, dependent libraries *must* be
  built with the ``(mode native)`` enabled. In addition to that, Coq must be
  configured to support native compilation. Dune explicitly disables the
  generation of native compilation objects when ``(mode vo)`` is enabled,
  irrespective of the configuration of Coq. This will be improved in the future.

.. _include-subdirs-coq:

Recursive Qualification of Modules
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you add:

.. code:: scheme

    (include_subdirs qualified)

to a :ref:`dune<dune-files>` file, Dune considers all the modules in the
directory and its subdirectories, adding a prefix to the module name in the
usual Coq style for subdirectories. For example, file ``A/b/C.v`` becomes the
module ``A.b.C``.

.. _public-private-theory:

Public and Private Theories
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A *public theory* is a :ref:`coq-theory` stanza that is visible outside the
scope of a :ref:`dune-project` file.

A *private theory* is a :ref:`coq-theory` stanza that is limited to the scope of
the :ref:`dune-project` file it is in.

A private theory may depend on both private and public theories; however, a
public theory may only depend on other public theories.

By default, all :ref:`coq-theory` stanzas are considered private by Dune. In
order to make a private theory into a public theory, the ``(package )`` field
must be specified.

.. code:: scheme

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
  plugins for things to work. This is a limitation of ``coqdep``.

- Building a theory and a plugin in the same directory can lead to issues with
  the presence of the META file. We recommend the following:
  - A separate directory for the files of each :ref:`coq-theory` stanza defined.
  - A separate directory for source files of a plugin.

.. _coq-lang:

Coq Language Version
~~~~~~~~~~~~~~~~~~~~

The Coq lang can be modified by adding the following to a :ref:`dune-project`
file:

.. code:: scheme

    (using coq 0.4)

The supported Coq language versions (not the version of Coq) are:

- ``0.1``: Basic Coq theory support.
- ``0.2``: Support for the ``theories`` field and composition of theories in the
  same scope.
- ``0.3``: Support for ``(mode native)`` requires Coq >= 8.10 (and Dune >= 2.9
  for Coq >= 8.14).
- ``0.4``: Support for interproject composition of theories.

.. _coq-lang-1.0:

Coq Language Version 1.0
~~~~~~~~~~~~~~~~~~~~~~~~

Guarantees with respect to stability are not yet provided. However, as the
development of features progresses, we hope to reach ``1.0`` soon. The ``1.0``
version of Coq lang will commit to a stable set of functionality. All the
features below are expected to reach ``1.0`` unchanged or minimally modified.

.. _coq-extraction:

coq.extraction
--------------

Coq may be instructed to *extract* OCaml sources as part of the compilation
process by using the ``coq.extraction`` stanza:

.. code:: scheme

   (coq.extraction
    (prelude <name>)
    (extracted_modules <names>)
    <optional-fields>)

- ``(prelude <name>)`` refers to the Coq source that contains the extraction
  commands.

- ``(extracted_modules <names>)`` is an exhaustive list of OCaml modules
  extracted.

- ``<optional-fields>`` are ``flags``, ``theories``, and ``libraries``. All of
  these fields have the same meaning as in the ``coq.theory`` stanza.

The extracted sources can then be used in ``executable`` or ``library`` stanzas
as any other sources.

Note that the sources are extracted to the directory where the ``prelude`` file
lives. Thus the common placement for the ``OCaml`` stanzas is in the same
:ref:`dune<dune-files>` file.

**Warning**: using Coq's ``Cd`` command to work around problems with the output
directory is not allowed when using extraction from Dune. Moreover the ``Cd``
command has been deprecated in Coq 8.12.

.. _coq-pp:

coq.pp
------

Authors of Coq plugins often need to write ``.mlg`` files to extend the Coq
grammar. Such files are preprocessed with the ``coqpp`` binary. To help plugin
authors avoid writing boilerplate, we provide a ``(coqpp ...)`` stanza:

.. code:: scheme

    (coq.pp (modules <mlg_list>))

which, for each ``g_mod`` in ``<mlg_list>``, is equivalent to the following
rule:

.. code:: lisp

    (rule
     (targets g_mod.ml)
     (deps (:mlg-file g_mod.mlg))
     (action (run coqpp %{mlg-file})))

.. _examples:

Examples of Coq Projects
------------------------

Here we list some examples of some basic Coq project setups in order.

.. _example-simple:

Simple Project
~~~~~~~~~~~~~~

Let us start with a simple project. First, make sure we have a
:ref:`dune-project` file with a :ref:`Coq lang<coq-lang>` stanza present:

.. code:: scheme

  (lang dune 3.4)
  (using coq 0.4)

Next we need a :ref:`dune<dune-files>` file with a :ref:`coq-theory` stanza:

.. code:: scheme

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
  │   │   ├── A.v.d
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

Here are the :ref:`dune<dune-files>` files:

.. code:: scheme

  ; A/dune
  (include_subdirs qualified)
  (coq.theory
   (name A))

  ; B/dune
  (coq.theory
   (name B)
   (theories A))

Notice the ``theories`` field in ``B`` allows one :ref:`coq-theory` to depend on
another. Another thing to note is the inclusion of the :ref:`include_subdirs`
stanza. This allows our theory to have :ref:`multiple
subdirectories<include-subdirs-coq>`.

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

Our :ref:`dune<dune-files>` file in ``CombinedWork`` looks like:

.. code:: scheme

  (coq.theory
   (name Combined)
   (theories myTheory A B))

As you can see, there are dependencies on all the theories we mentioned.

All three of the theories we defined before were *private theories*. In order to
depend on them, we needed to make them *public theories*. See the section on
:ref:`public-private-theory`.

.. _running-coq-top:

Running a Coq Toplevel
----------------------

Dune supports running a Coq toplevel binary such as ``coqtop``, which is
typically used by editors such as CoqIDE or Proof General to interact with Coq.

The following command:

.. code:: bash

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

Limitations
~~~~~~~~~~~

* Only files that are part of a stanza can be loaded in a Coq toplevel.
* When a file is created, it must be written to the file system before the Coq
  toplevel is started.
* When new dependencies are added to a file (via a Coq ``Require`` vernacular
  command), it is in principle required to save the file and restart to Coq
  toplevel process.
