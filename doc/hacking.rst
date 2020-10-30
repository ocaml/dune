****************************
Working on the Dune codebase
****************************

This section gives guidelines for working on Dune itself. Many of
these are general guidelines that are specific to Dune. However, given
that Dune is a large project developed by many different people, it
is important to follow these guidelines when working on Dune in order
to keep the project in a good state and pleasant to work on for
everybody.

Writing tests
=============

Most of our tests are written as expectation style tests. While
writing such tests, the developer write some code and then let the
system insert the output produced during the execution of this code
right next to the code in the source file.

Once a test is written and committed, the system will check that the
captured output is still the one produced by a fresh execution of the
code. When the two don't match, the test is considered as failing and
the system displays a diff between what was expected and what the code
produced.

Both our unit tests and integration tests are written this way. For
unit tests, we use the ppx_expect_ framework where tests are
introduced via ``let%expect_test`` and expectation are capture in
``[%expect ...]`` nodes:

.. code:: ocaml

   let%expect_test "<test name>" =
      print_string "Hello, world!";
      [%expect {|
        Hello, world!
      |}]

For integration tests, we use a system similar to `cram tests
<https://bitheap.org/cram/>`_ for testing shell commands and their
behavior:

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

As with any long running software project, code written by one person
will always eventually end up being maintained by another. Just like
normal code, it is important to document tests. Especially since test
suites are most often composed of many individual tests that must be
understood on their own.

A well written test case should be easy to understand. A reader should
be able to quickly understand what property the test is checking, how
it is doing it and how to convince one-self that the test outcome is
the right one. A well written test will make it easy for future
maintainers to understand the test and react when the test
breaks. Most often, the code will need to be adapted to preserve the
existing behavior, however in some rare cases the test expectation
will need to be updated.

It is crucial that each test cases makes it purpose and logic crystal
clear so that future maintainers know how to deal with it.

When writing a test, we generally have a good idea of what we want to
test. Sometimes, we want to test that a new feature we developed is
behaving as we expect. Sometimes, we want to add a reproduction case
for a bug reported by a user to make sure future changes won't
re-introduce the faulty behaviour. Just like when programming, we turn
such an idea into code, which is a formal language that a computer can
understand. While another person reading this code might be able to
follow and understand what the code is doing step by step, it is not
clear that they will be able to reconstruct the original idea the
developer had in their mind when they originally wrote the code. What
is worse, they might understand the code in a completely different way
which would lead them to update it the wrong way.
