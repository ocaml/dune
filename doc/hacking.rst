****************************
Working on the Dune codebase
****************************

This section gives guidelines for working on Dune itself. Many of these are
general guidelines that are specific to Dune. However, given that Dune is a
large project developed by many different people, it is important to follow
these guidelines when working on Dune in order to keep the project in a good
state and pleasant to work on for everybody.

Writing tests
=============

Most of our tests are written as expectation style tests. While writing such
tests, the developer write some code and then let the system insert the output
produced during the execution of this code right next to the code in the source
file.

Once a test is written and committed, the system will check that the captured
output is still the one produced by a fresh execution of the code. When the two
don't match, the test is considered as failing and the system displays a diff
between what was expected and what the code produced.

Both our unit tests and integration tests are written this way. For unit tests,
we use the ppx_expect_ framework where tests are introduced via
``let%expect_test`` and expectation are capture in ``[%expect ...]`` nodes:

.. code:: ocaml

   let%expect_test "<test name>" =
      print_string "Hello, world!";
      [%expect {|
        Hello, world!
      |}]

For integration tests, we use a system similar to `cram tests
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
always eventually end up being maintained by another. Just like normal code, it
is important to document tests. Especially since test suites are most often
composed of many individual tests that must be understood on their own.

A well written test case should be easy to understand. A reader should be able
to quickly understand what property the test is checking, how it is doing it and
how to convince one-self that the test outcome is the right one. A well written
test will make it easy for future maintainers to understand the test and react
when the test breaks. Most often, the code will need to be adapted to preserve
the existing behavior, however in some rare cases the test expectation will need
to be updated.

It is crucial that each test cases makes it purpose and logic crystal clear so
that future maintainers know how to deal with it.

When writing a test, we generally have a good idea of what we want to test.
Sometimes, we want to test that a new feature we developed is behaving as we
expect. Sometimes, we want to add a reproduction case for a bug reported by a
user to make sure future changes won't re-introduce the faulty behaviour. Just
like when programming, we turn such an idea into code, which is a formal
language that a computer can understand. While another person reading this code
might be able to follow and understand what the code is doing step by step, it
is not clear that they will be able to reconstruct the original idea the
developer had in their mind when they originally wrote the code. What is worse,
they might understand the code in a completely different way which would lead
them to update it the wrong way.

Adding Stanzas
==============

Adding new stanzas is the most natural way to extend dune with new features.
Therefore we try to make this as easy as possible. The minimal amount of steps
to add a new stanza is:

- Extend ``Stanza.t`` with a new constructor to represent the new stanza
- Modify ``Dune_file`` to parse the dune language into this constructor
- Modify the rules  to interpret this stanza into rules. This is usually done in
  ``Gen_rules```

Versioning
----------

Dune is incredibly strict with versioning of new features, modifications that
are visible to the user, and changes to existing rules. This means that any
added stanza must be guarded behind the version of the dune language in which it
was introduced. For example:

.. code:: ocaml

   ; ( "cram"
     , let+ () = Dune_lang.Syntax.since Stanza.syntax (2, 7)
       and+ t = Cram_stanza.decode in
       [ Cram t ] )

Here the cram stanza was introduced in dune 2.7, so the user must enable ``(lang
dune 2.7)`` in their dune-project file to use it.

``since`` isn't the only primitive for making sure that versions are respected.
See ``Dune_lang.Syntax`` for other commonly used functions.

Experimental & Independent Extensions
-------------------------------------

Sometimes, dune's versioning policy is too strict. For example, it does not work
in the following situations:

- Mostly independent extensions of dune that only exist inside dune for
  development convenience. For example, build rules for coq. Such extensions
  would like to impose their own versioning policy.

- Experimental features that cannot yet guarantee dune's strict backwards
  compatibility. Such features may dropped or modified at any time.

To handle both of these use cases, dune allows to define new languages (with the
same syntax). These languages have their own versioning scheme and their own
stanzas (or fields). In dune itself, such languages are represented with
``Syntax.t`` Here's an example of how the coq syntax is defined:

.. code:: ocaml

   let coq_syntax =
     Dune_lang.Syntax.create ~name:"coq" ~desc:"the coq extension (experimental)"
      [ ((0, 1), `Since (1, 9)); ((0, 2), `Since (2, 5)) ]

The list provides which versions of the syntax are provided, and in which
version of dune they were introduced.

Such languages must be enabled in the dune-project separately:

.. code:: scheme

   (lang dune 2.8)
   (using coq 0.2)

If such extensions are experimental, it's recommended that they pass
``~experimental:true``, and that their versions are below 1.0.

It's also recommended that such extensions introduce stanzas or fields of the
form ``ext_name.stanza_name`` or ``ext_name.field_name`` to make it clear to the
user which extensions is providing a certain feature.

Dune Rules
==========

Creating Rules
--------------

A dune rule consists of 3 components:

- Dependencies that the rule may read when executed (files, aliases, ..)
  This is described by ``'a Build.t`` values

- Targets the rule produces (files)
  Targets, in addition to dependencies is described by ``'a Build.With_targets.t'``

- Action that dune must execute (external programs, redirects, etc.)
  Actions are represented by ``Action.t``

Combined, one needs to produce a ``Action.t Build.With_targets.t`` value to
create a rule. The rule may then be added by ``Super_context.add_rule``, or a
related function.

To make this maximally convenient, there's a ``Command`` module to make it
easier to create actions that run external commands and describe their targets &
dependencies simultaneously.

Loading Rules
-------------

Dune rules are loaded lazily to improve performance. Here's a sketch of the
algorithm that tries to load the rule that generates some target file `t`.

- Get the directory that of `t`. Call it `d`.

- Load all rules in `d` into a map from targets in that directory to rules that
  produce it.

- Look up the rule for `t` in this map.

To adhere to this loading scheme, our rules must therefore be generated as part
of the callback that generates targets in that directory. See the ``Gen_rules``
module for how this callback is constructed.
