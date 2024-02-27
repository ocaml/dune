##############################
 Working on the Dune Codebase
##############################

..
   TODO(diataxis)
   This can be folded either in a meta section or as an how-to guide.

This section gives guidelines for working on Dune itself. Many of these
are general guidelines specific to Dune. However, given that Dune is a
large project developed by many different people, it's important to
follow these guidelines in order to keep the project in a good state and
pleasant to work on for everybody.

.. contents:: Table of Contents
   :depth: 1
   :local:
   :backlinks: none

**************
 Dependencies
**************

To create a directory-local opam switch with the dependencies necessary
to build the tests, run:

.. code:: console

   $ make dev-switch

***************
 Bootstrapping
***************

Dune uses Dune as its build system, which requires some specific
commands to work. Running ``make dev`` bootstraps (if necessary) and
runs ``./dune.exe build @install``.

If you want to just run the bootstrapping step itself, build the
``bootstrap`` phony target with

.. code:: console

   $ make bootstrap

You can always rerun this to bootstrap again.

Once you've bootstrapped Dune, you should be using it to develop Dune
itself. Here are the most common commands you'll be running:

.. code:: console

   # to make sure everything compiles:
   $ ./dune.exe build @check
   # run all the tests
   $ ./dune.exe runtest
   # run a particular cram foo.t:
   $ ./dune.exe build @foo

Note that tests are currently written for version 4.14.1 of the OCaml
compiler. Some tests depend on the specific wording of compilation
errors which can change between compiler versions, so to reliably run
the tests make sure that ``ocaml.4.14.1`` is installed. The
``TEST_OCAMLVERSION`` in the ``Makefile`` at the root of the Dune repo
contains the current compiler version for which tests are written.

.. seealso::

   :doc:`explanation/bootstrap`

***************
 Writing Tests
***************

Most of our tests are written as expectation-style tests. While creating
such tests, the developer writes some code and then lets the system
insert the output produced during the code execution. The system puts it
right next to the code in the source file.

Once you write and commit a test, the system checks that the captured
output matches the one produced by a fresh code execution. When the two
don't match, the test fails. The system then displays a diff between
what was expected and what the code produced.

We write both our unit tests and integration tests in this way. For unit
tests, we use the ppx_expect_ framework, where we introduce tests via
``let%expect_test``, and ``[%expect ...]`` nodes capture expectations:

.. code:: ocaml

   let%expect_test "<test name>" =
      print_string "Hello, world!";
      [%expect {|
        Hello, world!
      |}]

For integration tests, we use a system similar to `Cram tests
<https://bitheap.org/cram/>`_ for testing shell commands and their
behavior:

.. code:: console

   $ echo 'Hello, world!'
   Hello, world!

   $ false
   [1]

   $ cat <<EOF
   > multi
   > line
   > EOF
   multi
   line

.. _ppx_expect: https://github.com/janestreet/ppx_expect

.. seealso::

   `actions_to_sh tests <https://github.com/ocaml/dune/blob/3.12.2/test/expect-tests/dune_engine/action_to_sh_tests.ml>`_
     An example of expect-tests.

   `mdx-stanza/locks.t <https://github.com/ocaml/dune/blob/3.12.2/test/blackbox-tests/test-cases/mdx-stanza/locks.t>`_
     An example of Cram test.

When running Dune inside tests, the ``INSIDE_DUNE`` environment variable
is set. This has the following effects:

-  Change the default root detection behaviour to use the current
   directory rather than the top most ``dune-project`` /
   ``dune-workspace`` file.

-  Be less verbose when Dune outputs a user message.

-  Error reporting is deterministic by default.

-  Prefer not to use a diff program for displaying diffs.

This list is not exhaustive and may change in the future. In order to
find the exact behaviour, it is recommended to search for
``INSIDE_DUNE`` in the codebase.

Guidelines
==========

As with any long running software project, code written by one person
will eventually be maintained by another. Just like normal code, it's
important to document tests, especially since test suites are most often
composed of many individual tests that must be understood on their own.

A well-written test case should be easily understood. A reader should be
able to quickly understand what property the test is checking, how it's
doing it, and how to convince oneself that the test outcome is the right
one. A well-written test makes it easier for future maintainers to
understand the test and react when the test breaks. Most often, the code
will need to be adapted to preserve the existing behavior; however, in
some rare cases, the test expectation will need to be updated.

It's crucial that each test case makes its purpose and logic crystal
clear, so future maintainers know how to deal with it.

When writing a test, we generally have a good idea of what we want to
test. Sometimes, we want to ensure a newly developed feature behaves as
expected. Other times, we want to add a reproduction case for a bug
reported by a user to ensure future changes won't reintroduce the faulty
behaviour. Just like when programming, we turn such an idea into code,
which is a formal language that a computer can understand. While another
person reading this code might be able to follow and understand what the
code does step by step, it isn't clear that they'll be able to
reconstruct the original developer's idea. Even worse, they might
understand the code in a completely different way, which would lead them
to update it incorrectly.

***************************************************
 Setting Up Your Development Environment Using Nix
***************************************************

You can use Nix to setup the development environment. This can be done
by running ``nix develop`` in the root of the Dune repository.

Note that Dune only takes OCaml as a dependency and the rest of the
dependencies are used when running the test suite.

Running ``nix develop`` can take a while the first time, therefore it is
advisable to save the state in a profile.

```sh nix develop --profile nix/profiles/dune ```

And to load the profile:

```sh nix develop nix/profiles/dune ```

This profile might need to be updated from time to time, since the
bootstrapped version of Dune may become stale. This can be done by
running the first command.

We have the following shells for specific tasks:

-  ``nix develop .#slim`` for a dev environment with fewer dependencies
   that is faster to build.

-  ``nix develop .#slim-melange``: same as above, but additionally
   includes the ``melange`` and ``mel`` packages

-  Building documentation requires ``nix develop .#doc``.

-  For running the Coq tests, you can use ``nix develop .#coq``. NB: Coq
   native is not currently installed; this will cause some of the tests
   to fail. It's currently better to fallback to opam in this case.

****************
 Releasing Dune
****************

Dune's release process relies on dune-release_. Make sure you install
and understand how this software works before proceeding. Publishing a
release consists of two steps:

-  Updating ``CHANGES.md`` to reflect the version being published.
-  Running ``$ make opam-release`` to create the release tarball. Then
   publish it to GitHub and submit it to opam.

Major & Feature Releases
========================

Given a new version ``x.y.z``, a major release increments ``x``, and a
feature release increments ``y``. Such a release must be done from the
``main`` branch. Once you publish the release, be sure to publish a
release branch named ``x.y``.

Point Releases
==============

Point releases increment the ``z`` in ``x.y.z``. Such releases are done
from the respective ``x.y`` branch of the respective feature release.
Once released, be sure to update ``CHANGES.md`` in the ``main`` branch.

****************
 Adding Stanzas
****************

Adding new stanzas is the most natural way to extend Dune with new
features. Therefore, we try to make this as easy as possible. The
minimal amount of steps to add a new stanza is:

-  Extend ``Stanza.t`` with a new constructor to represent the new
   stanza
-  Modify ``Dune_file`` to parse the Dune language into this constructor
-  Modify the rules to interpret this stanza into rules, usually done in
   ``Gen_rules```

Versioning
==========

Dune is incredibly strict with versioning of new features, modifications
visible to the user, and changes to existing rules. This means that any
added stanza must be guarded behind the version of the Dune language in
which it was introduced. For example:

.. code:: ocaml

   ; ( "cram"
     , let+ () = Dune_lang.Syntax.since Stanza.syntax (2, 7)
       and+ t = Cram_stanza.decode in
       [ Cram t ] )

Here, Dune 2.7 introduced the Cram stanza, so the user must enable
``(lang dune 2.7)`` in their ``dune`` project file to use it.

``since`` isn't the only primitive for making sure that versions are
respected. See ``Dune_lang.Syntax`` for other commonly used functions.

Experimental & Independent Extensions
=====================================

Sometimes, Dune's versioning policy is too strict. For example, it
doesn't work in the following situations:

-  When most Dune independent extensions only exist inside Dune for
   development convenience, e.g., build rules for Coq. Such extensions
   would like to impose their own versioning policy.

-  When experimental features cannot guarantee Dune's strict backwards
   compatibility. Such features may dropped or modified at any time.

To handle both of these use cases, Dune allows the definition of new
languages (with the same syntax). These languages have their own
versioning scheme and their own stanzas (or fields). In Dune itself,
``Syntax.t`` represents such languages. Here's an example of how the Coq
syntax is defined:

.. code:: ocaml

   let coq_syntax =
     Dune_lang.Syntax.create ~name:"coq" ~desc:"the coq extension (experimental)"
      [ ((0, 1), `Since (1, 9)); ((0, 2), `Since (2, 5)) ]

The list provides which versions of the syntax are provided and which
version of Dune introduced them.

Such languages must be enabled in the ``dune`` project file separately:

.. code:: dune

   (lang dune 3.14)
   (using coq 0.8)

If such extensions are experimental, it's recommended that they pass
``~experimental:true``, and that their versions are below 1.0.

We also recommend that such extensions introduce stanzas or fields of
the form ``ext_name.stanza_name`` or ``ext_name.field_name`` to clarify
which extensions provide a certain feature.

************
 Dune Rules
************

Creating Rules
==============

A Dune rule consists of 3 components:

-  *Dependencies* that the rule may read when executed (files, aliases,
   etc.), described by ``'a Action_builder.t`` values.
-  *Targets* that the rule produces (files and/or directories),
   described by ``'a Action_builder.With_targets.t'`` values.
-  *Action* that Dune must execute (external programs, redirects, etc.).
   Actions are represented by ``Action.t`` values.

Combined, one needs to produce an ``Action.t
Action_builder.With_targets.t`` value to create a rule. The rule may
then be added by ``Super_context.add_rule`` or a related function.

To make this maximally convenient, there's a ``Command`` module to make
it easier to create actions that run external commands and describe
their targets and dependencies simultaneously.

Loading Rules
=============

Dune rules are loaded lazily to improve performance. Here's a sketch of
the algorithm that tries to load the rule that generates some target
file ``t``.

-  Get the directory that contains ``t``. Call it ``d``.
-  Load all rules in ``d`` into a map from targets in that directory to
   rules that produce it.
-  Look up the rule for ``t`` in this map.

To adhere to this loading scheme, we must generate our rules as part of
the callback that creates targets in that directory. See the
``Gen_rules`` module for how this callback is constructed.

***************
 Documentation
***************

User documentation lives in the ``./doc`` directory.

In order to build the user documentation, you must install
python-sphinx_, sphinx_rtd_theme_ and sphinx-copybutton_.

Build the documentation with

.. code:: console

   $ make doc

For automatically updated builds, you can install sphinx-autobuild_, and
run

.. code:: console

   $ make livedoc

.. _dune-release: https://github.com/ocamllabs/dune-release

.. _python-sphinx: http://www.sphinx-doc.org/en/master/usage/installation.html

.. _sphinx-autobuild: https://pypi.org/project/sphinx-autobuild/

.. _sphinx-copybutton: https://sphinx-copybutton.readthedocs.io/en/latest/index.html

.. _sphinx_rtd_theme: https://sphinx-rtd-theme.readthedocs.io/en/stable/

Nix users may drop into a development shell with the necessary
dependencies for building docs ``nix develop .#doc``.

Structure
=========

For structure, we use the `Diátaxis framework`_. The core idea is that
documents should fit in one of the following categories:

.. _diátaxis framework: https://diataxis.fr/

-  Tutorials, focused on learning
-  How-to guides, focused on task solving
-  Reference, focused on information
-  Explanations, focused on understanding

Most features do not need a document in each category, but the important
part is that a single document should not try to be in several
categories at once.

ReStructured Text
=================

For code blocks containing Dune files, use ``.. code:: dune`` and indent
with 3 spaces. Use formatting consistent with how Dune formats Dune
files (most importantly, do not leave orphan closing parentheses).

In a document that only contains Dune code blocks, it is possible to use
the ``.. highlight:: dune`` directive to have ``dune`` be the default
lexer, and then it is possible to use the ``::`` shortcut to end a line
with a single ``:`` and start a code block. See the source of
:doc:`reference/lexical-conventions` for an example.

For links, prefer references that use ``:doc:`` (link to a whole
document) or ``:term:`` (link to a definition in the glossary) to
``:ref:``.

Use the right lexers: - ``dune`` for ``dune`` and related files -
``opam`` for opam files - ``console`` for shell sessions and commands
(start with ``$``) - ``cram`` for cram tests

Style
=====

Use American spelling.

Use `Title Case`_ for titles and headings (every word except "little
words" like of, and, or, etc.).

.. _title case: https://apastyle.apa.org/style-grammar-guidelines/capitalization/title-case

For project names, use the following capitalization:

-  **Dune** is the project, ``dune`` is the command. Files are called
   ``dune`` files.

-  ``dune-project`` should always be written in monospace.

-  **OCaml**

-  **OCamlFormat**, and ``ocamlformat`` is the command.

-  ``odoc``, always in monospace.

-  **opam**. Can be capitalised as Opam at the beginning of sentences
   only, as the official name is formatted opam. Even in titles,
   headers, and subheaders, it should be all lowercase: opam. The
   command is ``opam``.

-  **esy**. Can be capitalised as Esy.

-  **Nix**. The command is ``nix``.

-  **Js_of_ocaml** can be abbreviated **JSOO**.

-  **MDX**, rather than mdx or Mdx

-  **PPX,** rather than ppx or Ppx; ``ppxlib``

-  **UTop,** rather than utop or Utop.

***********
 Vendoring
***********

Dune vendors some code that it uses internally. This is done to make
installing Dune easy as it requires nothing but an OCaml compiler as
well as to prevent circular dependencies. Before vendoring, make sure
that the license of the code allows it to be included in Dune.

The vendored code lives in the ``vendor/`` subdirectory. To vendor new
code, create a shell script ``update-<library>.sh``, that will be
launched from the ``vendor/`` folder to download and unpack the source
and copy the necessary source files into the ``vendor/<library>``
folder. Try to keep the amount of source code imported minimal, e.g.,
leave out ``dune-project`` files. For the most part, it should be enough
to copy ``.ml`` and ``.mli`` files. Make sure to also include the
license if there is such a file in the code to be vendored to stay
compliant.

As these sources get vendored not as subprojects but parts of Dune, you
need to deal with ``public_name``. The preferred way is to remove the
``public_name`` and only use the private name. If that is not possible,
the library can be renamed into ``dune-private-libs.<library>``.

To deal with the modified ``dune`` files in ``update-<library>.sh``
scripts, you can commit the modified files to ``dune`` and make the
``update-<library>.sh`` script to use ``git checkout`` to restore the
``dune`` file.

For larger modifications, it is better to fork the upstream project in
the ocaml-dune_ organisation and then vendor the forked copy in Dune.
This makes the changes better visible and easier to update from upstream
in the long run while keeping our custom patches in sync. The changes to
the ``dune`` files are to be kept in the Dune repository.

It is preferable to cut out as many dependencies as possible, e.g., ones
that are only necessary on older OCaml versions or build-time
dependencies.

.. _ocaml-dune: https://github.com/ocaml-dune/

********************
 General Guidelines
********************

Dune has grown to be a fairly large project that over time has acquired
its own style. Below is an attempt to enumerate some important points of
this style. These rules aren't axioms and we may break them when
justified. However, we should have a good reason in mind when breaking
them. Finally, the list isn't exhaustive by any means and is subject to
change. Feel free to discuss anything in particular with the team.

-  Parameter signatures should be self descriptive. Use labels when the
   types alone aren't sufficient to make the signature readable.

Bad:

.. code:: ocaml

   val display_name : string -> string -> _ Pp.t

Good:

.. code:: ocaml

   val display_name : first_name:string -> last_name:string -> _ Pp.t

-  Avoid type aliases when possible. Yes, they might make some type
   signatures more readable, but they make the code harder to grep and
   make Merlin's inferred types more confusing.

-  Every ``.ml`` file must have a corresponding ``.mli``. The only
   exception to this rule is ``.ml`` files with only type definitions.

-  Do not write ``.mli`` only modules. They offer no advantages to
   ``.ml`` modules with type definitions and one cannot define
   exceptions in ``.mli`` only modules

-  Every module should have toplevel documentation that describes the
   module briefly. This is a good place to discuss its purpose,
   invariants, etc.

-  Keep interfaces short & sweet. The less functions, types, etc., there
   are, the easier it is for users to understand, use, and ultimately
   modify the interface correctly. Instead of creating elaborate
   interfaces with the hope of future-proofing every use case, embrace
   change and make it easier to throw out or replace the interface.

   Ideally the interface should have one obvious way to use it. A
   particularly annoying violator of this principle is the "logic-less
   chain of functions" helper. For example:

.. code:: ocaml

   let foo t = bar t |> baz

If ``bar`` and ``baz`` are already public, then there's no need to add
yet another helper to save the caller a line of code.

-  Define bindings as close to their use site as possible. When they're
   far apart, reading code requires scrolling and IDE tools to
   understand the code.

Bad:

.. code:: ocaml

   let dir = .. in
   (* 50 odd lines or so that don't use [dir] *)
   f dir

Good:

.. code:: ocaml

   let dir = .. in
   f dir

-  A corollary to the previous guideline: keep the scope of bindings as
   small as possible.

Bad:

.. code:: ocaml

   let x1 = f foo in let x2 = f bar in
   let y1 = g foo in let y2 = g bar in
   let dx = x2 -. x1 in
   let dy = y2 -. y1 in
   dx^2 +. dy^2

Good:

.. code:: ocaml

   let dx =
     let x1 = f foo in let x2 = f bar in
     x2 -. x1
   in
   let dy =
     let y1 = g foo in let y2 = g bar in
     y2 -. y1
   in
   dx^2 +. dy^2

-  Prefer ``Code_error.raise`` instead of ``assert false``. The reader
   often has no idea what invariant is broken by the ``assert false``.
   Kindly describe it to the reader in the error message.

-  Avoid meaningless names like ``x``, ``a``, ``b``, ``f``. Try to find
   a more descriptive name or just inline it altogether.

-  If a module ``Foo`` has a module type ``Foo.S`` and you'd like to
   avoid repeating its definition in the implementation and the
   signature, introduce an ``.ml``-only module ``Foo_intf`` and write
   the ``S`` only once in there.

-  Instead of introducing a type ``foo``, consider introducing a module
   ``Foo`` with a type ``t``. This is often the place to put functions
   related to ``foo``.

-  Avoid optional arguments. They increase brevity at the expense of
   readability and are annoying to grep. Furthermore, they encourage
   callers not to think at all about these optional arguments even if
   they often should.

-  Avoid qualifying modules when accessing fields of records or
   constructors. Avoid it altogether if possible, or add a type
   annotation if necessary.

Bad:

.. code:: ocaml

   let result = A.b () in
   match result.A.field with
   | B.Constructor -> ...

Good:

.. code:: ocaml

   let result : A.t = A.b () in
   match (result.field : B.t) with
   | Constructor -> ...

-  When constructing records, use the qualified names in in the record.
   Do not open the record. The local open syntax pulls in all kinds of
   names from the opened module and might shadow the values that you're
   trying to put into the record, leading to difficult debugging.

Bad; if ``A.value`` exists, it will pick that over ``value``:

.. code:: ocaml

   let value = 42 in
   let record = A.{ field = value; other } in
   ...

Good:

.. code:: ocaml

   let value = 42 in
   let record = { A.field = value; other } in
   ...

-  Stage functions explicitly with the ``Staged`` module.

-  Do not raise ``Invalid_argument``. Instead, raise with
   ``Code_error.raise`` which allows to attach more informative payloads
   than just strings.

-  When ignoring the value of a let binding ``let _ = ...``, we add type
   annotations to the ignored value ``let (_ : t) = ...``. We do this
   convention because:

   -  We need to make sure we never ignore ``Fiber.t`` accidentally.
      Functions that return ``Fiber.t`` are always free of side effects
      so we need to bind on the result to force the side effect.

   -  Whenever a function is changed to return an error via its return
      value, we want the compiler to notify all the callers that need to
      be updated.

-  To write a ``to_dyn`` function on a record type, use the following
   pattern. It ensures that the pattern matching will break when a field
   is added. To ignore a field, add ``; d = _``, not ``; _``.

.. code:: ocaml

   let to_dyn {a; b; c} =
     Dyn.record
       [ ("a", A.to_dyn a)
       ; ("b", B.to_dyn b)
       ; ("c", C.to_dyn c)
       ]

-  To write an equality function, use the following pattern (this
   applies to other kinds of binary functions). The same remarks about
   about pattern matching and ignoring fields apply.

.. code:: ocaml

   let equal {a; b; c} t =
     A.equal a t.a &&
     B.equal b t.b &&
     C.equal c t.c

Subjective Style Points
=======================

There's some stylistic decisions we made that don't have logical
justification and are basically a matter of taste. Nevertheless, it's
useful to follow them to keep the code consistent.

-  Match patterns should be sorted by the length of their RHS when
   possible. Keep the shorter clauses near the top.

-  If a module ``Foo`` defines a type ``t``, all functions that take
   ``t`` in this module should have ``t`` as their first argument. This
   is the "t comes first" rule.

-  Do not mix ``|>`` and ``@@`` in the same expression.

-  Introduce bindings that will allow opportunities for record or label
   punning.

-  Do not write inverted if-else expressions.

Bad:

.. code:: ocaml

   (* try reading this out loud without short circuiting your brain *)
   if not x then foo else bar

Good:

.. code:: ocaml

   if x then bar else foo

-  We prefer snake_casing identifiers. This includes the names of
   modules and module types.
-  Avoid qualifying constructors and record fields. Instead, add type
   annotations to the type being matched on or being constructed, e.g.,

Bad:

.. code:: ocaml

   let foo = Command.Args.S []

Good:

.. code:: ocaml

   let (foo : _ Command.Args.t) = S []

**************
 Benchmarking
**************

Dune Bench
==========

You can benchmark Dune's performance by running ``make bench``. This
will run a subset of the Duniverse. If you are running the bench
locally, make sure that you bootstrap since that is the executable that
the bench will run.

The bench will build a specially selected portion of the Duniverse once,
called a "clean build". Afterwards, the build will be run 5 more times
and are termed the "Null builds".

In each run of the CI, there will be an ``ocaml-benchmarks`` status in
the summary. Clicking ``Details`` will show a bench report.

The report contains the following information:

-  The build times for Clean and Null builds
-  The size of the ``dune.exe`` binary
-  User CPU times for the Clean and Null builds
-  System CPU times for the Clean and Null builds
-  All the garbage collection stats apart from "forced collections" for
   Clean and Null builds

Pull requests that add new libraries are likely to increase the size of
the dune binary.

Performance gains in Dune can be observed in the Clean and Null build
times.

Memory usage can be observed in the garbage collection stats.

Inline Benchmarks
=================

Certain performance-critical parts of Dune are benchmarked using the
``inline_benchmarks`` library. These benchmarks are run when running the
tests. Their outputs are currently not recorded and are only used to
detect performance regressions.

Build-Time Benchmarks
=====================

We benchmark the build time of Dune in every PR. The times can be found
here:

https://autumn.ocamllabs.io/ocaml/dune?worker=autumn&image=bench.Dockerfile

Melange Bench
=============

We also benchmark a demo Melange project's build time:

https://ocaml.github.io/dune/dev/bench/

Monorepo Benchmark
==================

The file bench/monorepo/bench.Dockerfile sets up a Docker container for
benchmarking Dune building a large monorepo constructed with
`opam-monorepo <https://github.com/tarides/opam-monorepo>`_. The
monorepo is constructed according to the files in
https://github.com/ocaml-dune/ocaml-monorepo-benchmark/tree/main/benchmark.
Build the Docker image from the root directory of this repo.

E.g., run:

.. code:: console

   $ docker build . -f bench/monorepo/bench.Dockerfile --tag=dune-monorepo-benchmark

The monorepo benchmark Docker image requires ``duniverse`` directory to
be mounted as a volume. Generate this directory with a script from the
`ocaml-monorepo-benchmark
<https://github.com/ocaml-dune/ocaml-monorepo-benchmark>`_ repository:

.. code:: console

   $ git clone https://github.com/ocaml-dune/ocaml-monorepo-benchmark.git
   $ cd ocaml-monorepo-benchmark
   $ ./generate-duniverse.sh /tmp

This will create a directory ``/tmp/duniverse``. Then to run the
benchmark, run the Docker image in a container mounting
``/tmp/duniverse`` as a volume at
``/home/opam/bench-dir/current-bench-data/duniverse`` (that specific
path is a requirement of `current-bench
<https://github.com/ocurrent/current-bench>`_). From within the
container the benchmarks can be started by running `make bench`. Do all
this in a single command with:

.. code:: console

   $ docker run -it --volume=/tmp/duniverse:/home/opam/bench-dir/current-bench-data/duniverse dune-monorepo-benchmark bash --login -c 'make bench'

The benchmark will print out a JSON string in the format accepted by
`current-bench <https://github.com/ocurrent/current-bench>`_.

Read more at
https://github.com/ocaml/dune/blob/main/bench/monorepo/README.md.

************
 Formatting
************

When changing the formatting configuration, it is possible to add the
reformatting commit to the :file:`.git-blame-ignore-revs` file. The
commit will disappear from blame views. It is also possible to configure
``git`` to have the same behavior locally.

It is recommended to edit that file in a second PR, to make sure that
the referenced commit has not changed.

.. seealso::

   `GitHub - Ignore commits in the blame view
   <https://docs.github.com/en/repositories/working-with-files/using-files/viewing-a-file#ignore-commits-in-the-blame-view>`_
