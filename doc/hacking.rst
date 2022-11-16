****************************
Working on the Dune Codebase
****************************

This section gives guidelines for working on Dune itself. Many of these are
general guidelines specific to Dune. However, given that Dune is a large project
developed by many different people, it's important to follow these guidelines in
order to keep the project in a good state and pleasant to work on for everybody.

.. contents:: Table of Contents
   :depth: 1
   :local:
   :backlinks: none

Bootstrapping
=============

In order to build itself, Dune uses a micro dune written as a single
``boot/duneboot.ml`` file. This micro build system cannot read ``dune`` files
and instead has the configuration hard-coded in ``boot/libs.ml``. This latter
file is automatically updated during development when we modify the ``dune``
files in the repository. ``boot/duneboot.ml`` itself is built with a single
invocation of ``ocamlopt`` or ``ocamlc`` via the ``bootstrap.ml`` OCaml script.

``boot/duneboot.ml`` builds a ``dune.exe`` binary in the ``_boot`` directory
and uses this binary to build everything else. As a convenience, ``dune.exe``
at the root of the source tree executes this binary.

``$ make dev`` takes care of bootstrapping if needed, but if you want to just
run the bootstrapping step itself, build the ``dune.exe`` target with

.. code:: sh

   make dev

Once you've bootstrapped dune, you should be using it to develop dune itself.
Here are the most common commands you'll be running:

.. code:: sh

   # to make sure everything compiles:
   $ ./dune.exe build @check
   # run all the tests
   $ ./dune.exe runtest
   # run a particular cram foo.t:
   $ ./dune.exe build @foo

Writing Tests
=============

Most of our tests are written as expectation-style tests. While creating such
tests, the developer writes some code and then lets the system insert the output
produced during the code execution. The system puts it right next to the code in
the source file.

Once you write and commit a test, the system checks that the captured output
matches the one produced by a fresh code execution. When the two don't match,
the test fails. The system then displays a diff between what was expected and
what the code produced.

We write both our unit tests and integration tests in this way. For unit tests,
we use the ppx_expect_ framework, where we introduce tests via
``let%expect_test``, and ``[%expect ...]`` nodes capture expectations:

.. code:: ocaml

   let%expect_test "<test name>" =
      print_string "Hello, world!";
      [%expect {|
        Hello, world!
      |}]

For integration tests, we use a system similar to `Cram tests
<https://bitheap.org/cram/>`_ for testing shell commands and their behavior:

.. code:: bash

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

.. _ppx_expect:      https://github.com/janestreet/ppx_expect

Guidelines
----------

As with any long running software project, code written by one person will
eventually be maintained by another. Just like normal code, it's important to
document tests, especially since test suites are most often composed of many
individual tests that must be understood on their own.

A well-written test case should be easily understood. A reader should be able to
quickly understand what property the test is checking, how it's doing it, and
how to convince oneself that the test outcome is the right one. A well-written
test makes it easier for future maintainers to understand the test and react
when the test breaks. Most often, the code will need to be adapted to preserve
the existing behavior; however, in some rare cases, the test expectation will
need to be updated.

It's crucial that each test case makes its purpose and logic crystal clear, so
future maintainers know how to deal with it.

When writing a test, we generally have a good idea of what we want to test.
Sometimes, we want to ensure a newly developed feature behaves as expected.
Other times, we want to add a reproduction case for a bug reported by a user to
ensure future changes won't reintroduce the faulty behaviour. Just like when
programming, we turn such an idea into code, which is a formal language that a
computer can understand. While another person reading this code might be able to
follow and understand what the code does step by step, it isn't clear that
they'll be able to reconstruct the original developer's idea. Even worse, they
might understand the code in a completely different way, which would lead them
to update it incorrectly.

Setting Up Your Development Environment Using Nix
=================================================

You can use Nix to setup the development environment. This can be done by
running ``nix develop`` in the root of the Dune repository.

Note that Dune only takes OCaml as a dependency and the rest of the dependencies
are used when running the test suite.

Running ``nix develop`` can take a while the first time, therefore it is
advisable to save the state in a profile.

```sh
nix develop --profile nix/profiles/dune
```

And to load the profile:

```sh
nix develop nix/profiles/dune
```

This profile might need to be updated from time to time, since the bootstrapped
version of Dune may become stale. This can be done by running the first command.

You may also use `nix develop .#slim` for a dev environment with less
dependencies that is faster to build.

Releasing Dune
==============

Dune's release process relies on dune-release_. Make sure you install and
understand how this software works before proceeding. Publishing a release
consists of two steps:

* Updating ``CHANGES.md`` to reflect the version being published.
* Running ``$ make opam-release`` to create the release tarball. Then publish it
  to GitHub and submit it to opam.

Major & Feature Releases
------------------------

Given a new version `x.y.z`, a major release increments `x`, and a feature
release increments `y`.  Such a release must be done from the `main` branch.
Once you publish the release, be sure to publish a release branch named `x.y`.

Point Releases
--------------

Point releases increment the `z` in `x.y.z`. Such releases are done from the
respective `x.y` branch of the respective feature release. Once released, be
sure to update `CHANGES` in the `main` branch.

Adding Stanzas
==============

Adding new stanzas is the most natural way to extend Dune with new features.
Therefore, we try to make this as easy as possible. The minimal amount of steps
to add a new stanza is:

- Extend ``Stanza.t`` with a new constructor to represent the new stanza
- Modify ``Dune_file`` to parse the Dune language into this constructor
- Modify the rules to interpret this stanza into rules, usually done in
  ``Gen_rules```

Versioning
----------

Dune is incredibly strict with versioning of new features, modifications visible
to the user, and changes to existing rules. This means that any added stanza
must be guarded behind the version of the Dune language in which it was
introduced. For example:

.. code:: ocaml

   ; ( "cram"
     , let+ () = Dune_lang.Syntax.since Stanza.syntax (2, 7)
       and+ t = Cram_stanza.decode in
       [ Cram t ] )

Here, Dune 2.7 introduced the Cram stanza, so the user must enable
``(lang dune 2.7)`` in their ``dune`` project file to use it.

``since`` isn't the only primitive for making sure that versions are respected.
See ``Dune_lang.Syntax`` for other commonly used functions.

Experimental & Independent Extensions
-------------------------------------

Sometimes, Dune's versioning policy is too strict. For example, it doesn't work
in the following situations:

- When most Dune independent extensions only exist inside Dune for development
  convenience, e.g., build rules for Coq. Such extensions would like to impose
  their own versioning policy.

- When experimental features cannot guarantee Dune's strict backwards
  compatibility. Such features may dropped or modified at any time.

To handle both of these use cases, Dune allows the definition of new languages
(with the same syntax). These languages have their own versioning scheme and
their own stanzas (or fields). In Dune itself, ``Syntax.t`` represents such
languages. Here's an example of how the Coq syntax is defined:

.. code:: ocaml

   let coq_syntax =
     Dune_lang.Syntax.create ~name:"coq" ~desc:"the coq extension (experimental)"
      [ ((0, 1), `Since (1, 9)); ((0, 2), `Since (2, 5)) ]

The list provides which versions of the syntax are provided and which version of
Dune introduced them.

Such languages must be enabled in the ``dune`` project file separately:

.. code:: scheme

   (lang dune 3.7)
   (using coq 0.2)

If such extensions are experimental, it's recommended that they pass
``~experimental:true``, and that their versions are below 1.0.

We also recommend that such extensions introduce stanzas or fields of the form
``ext_name.stanza_name`` or ``ext_name.field_name`` to clarify which extensions
provide a certain feature.

Dune Rules
==========

Creating Rules
--------------

A Dune rule consists of 3 components:

- *Dependencies* that the rule may read when executed (files, aliases, etc.),
  described by ``'a Action_builder.t`` values.

- *Targets* that the rule produces (files and/or directories), described by
  ``'a Action_builder.With_targets.t'`` values.

- *Action* that Dune must execute (external programs, redirects, etc.). Actions
  are represented by ``Action.t`` values.

Combined, one needs to produce an ``Action.t Action_builder.With_targets.t``
value to create a rule. The rule may then be added by ``Super_context.add_rule``
or a related function.

To make this maximally convenient, there's a ``Command`` module to make it
easier to create actions that run external commands and describe their targets
and dependencies simultaneously.

Loading Rules
-------------

Dune rules are loaded lazily to improve performance. Here's a sketch of the
algorithm that tries to load the rule that generates some target file `t`.

- Get the directory that of `t`. Call it `d`.

- Load all rules in `d` into a map from targets in that directory to rules that
  produce it.

- Look up the rule for `t` in this map.

To adhere to this loading scheme, we must generate our rules as part of the
callback that creates targets in that directory. See the ``Gen_rules`` module
for how this callback is constructed.

Documentation
=============

User documentation lives in the ``./doc`` directory.

In order to build the user documentation, you must install python-sphinx_ and
sphinx_rtd_theme_.

Build the documentation with

.. code:: sh

   $ make doc

For automatically updated builds, you can install sphinx-autobuild, and run

.. code:: sh

   $ make livedoc

.. _python-sphinx: http://www.sphinx-doc.org/en/master/usage/installation.html
.. _sphinx_rtd_theme: https://sphinx-rtd-theme.readthedocs.io/en/stable/
.. _sphinx-autobuild: https://pypi.org/project/sphinx-autobuild/
.. _dune-release: https://github.com/ocamllabs/dune-release

Nix users may drop into a development shell with the necessary dependencies for
building docs ``nix develop .#doc``.

General Guidelines
==================

Dune has grown to be a fairly large project that over time has acquired its own
style. Below is an attempt to enumerate some important points of this style.
These rules aren't axioms and we may break them when justified. However, we
should have a good reason in mind when breaking them. Finally, the list isn't
exhaustive by any means and is subject to change. Feel free to discuss anything
in particular with the team.

- Parameter signatures should be self descriptive. Use labels when the types
  alone aren't sufficient to make the signature readable.

Bad:

.. code:: ocaml

   val display_name : string -> string -> _ Pp.t

Good:

.. code:: ocaml

   val display_name : first_name:string -> last_name:string -> _ Pp.t

- Avoid type aliases when possible. Yes, they might make some type signatures
  more readable, but they make the code harder to grep and make Merlin's
  inferred types more confusing.

- Every ``.ml`` file must have a corresponding ``.mli``. The only exception to
  this rule is ``.ml`` files with only type definitions.

- Do not write ``.mli`` only modules. They offer no advantages to ``.ml``
  modules with type definitions and one cannot define exceptions in ``.mli``
  only modules

- Every module should have toplevel documentation that describes the module
  briefly. This is a good place to discuss its purpose, invariants, etc.

- Keep interfaces short & sweet. The less functions, types, etc. there are, the
  easier it is for users to understand, use, and ultimately modify the
  interface correctly. Instead of creating elaborate interfaces with the hope
  of future-proofing every use case, embrace change and make it easier to throw
  out or replace the interface.

  Ideally the interface should have one obvious way to use it. A particularly
  annoying violator of this principle is the "logic-less chain of functions"
  helper. For example:

.. code:: ocaml

   let foo t = bar t |> baz

  If ``bar`` and ``baz`` are already public, then there's no need to add yet
  another helper to save the caller a line of code.

- Define bindings as close to their use site as possible. When they're far
  apart, reading code requires scrolling and IDE tools to understand the code.

Bad:

.. code:: ocaml

   let dir = .. in
   (* 50 odd lines or so that don't use [dir] *)
   f dir

Good:

.. code:: ocaml

  let dir = .. in
  f dir

- A corollary to the previous guideline: keep the scope of bindings as small as
  possible.

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

- Prefer ``Code_error.raise`` instead of ``assert false``. The reader often has
  no idea what invariant is broken by the ``assert false``. Kindly describe it
  to the reader in the error message.

- Avoid meangingless names like `x`, `a`, `b`, `f`. Try to find a more
  descriptive name or just inline it altogether.

- If a module ``Foo`` has a module type ``Foo.S`` and you'd like to avoid
  repeating its definition in the implementation and the signature, introduce
  an ``.ml``-only module ``Foo_intf`` and write the ``S`` only once in there.

- Instead of introducing a type ``foo``, consider introducing a module ``Foo``
  with a type ``t``. This is often the place to put functions related to
  ``foo``.

- Avoid optional arguments. They increase brevity at the expense of readability
  and are annoying to grep. Further more, they encourage callers not to think
  at all about these optional arguments even if they often should.

- Stage functions explicitly with the ``Staged`` module.

- Do not raise ``Invalid_argument``. Instead, raise with ``Code_error.raise``
  which allows to attach more informative payloads than just strings.

Subjective Style Points
-----------------------

There's some stylistic decisions we made that don't have logical justification
and are basically a matter of taste. Nevertheless, it's useful to follow them
to keep the code consistent.

- Match patterns should be sorted by the length of their RHS when possible.
  Keep the shorter clauses near the top.

- If a module ``Foo`` defines a type ``t``, all functions that take ``t`` in
  this module should have ``t`` as their first argument. This is the "t comes
  first" rule.

- Do not mix ``|>`` and ``@@`` in the same expression.

- Introduce bindings that will allow opportunities for record or label punning.

- Do not write inverted if-else expressions. 

Bad:

.. code:: ocaml

   (* try reading this out loud without short circuiting your brain *)
   if not x then foo else bar

Good:

.. code:: ocaml

   if x then bar else foo

- We prefer snake_casing identifiers. This includes the names of modules and
  module types.

- Avoid qualifying constructors and record fields. Instead, add type
  annotations to the type being matched on or being constructed. E.g.

Bad:

.. code:: ocaml

   let foo = Command.Args.S []

Good:

.. code:: ocaml

   let (foo : _ Command.Args.t) = S []
