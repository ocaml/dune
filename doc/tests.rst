*************************
Writing and running tests
*************************

Dune tries to streamline the testing story as much as possible, so
that you can focus on the tests themselves and not bother with setting
up with various test frameworks.

In this section, we will explain the workflow to deal with tests in dune. In
particular we will see how to run the testsuite of a project, how to describe
your tests to dune and how to promote tests result as expectation.

We distinguish three kinds of tests:

* inline tests - written directly inside the ml files of a library

* custom tests - run an executable, possibly followed by an action such as
  diffing the produced output.

* cram tests - expect tests written in cram_ style.

Running tests
=============

Whatever the tests of a project are, the usual way to run tests with dune is to
call ``dune runtest`` from the shell (or the command alias ``dune test``). This
will run all the tests defined in the current directory and any sub-directory
recursively.

Note that in any case, ``dune runtest`` is simply a short-hand for building the
``runtest`` alias, so you can always ask dune to run the tests in conjunction
with other targets by passing ``@runtest`` to ``dune build``. For instance:

.. code:: bash

          $ dune build @install @runtest
          $ dune build @install @test/runtest


Running a single test
---------------------

If you would only like to run a single test for your project, you may use ``dune
exec`` to run the test executable (for the sake of this example,
``project/tests/myTest.ml``):

.. code:: bash

   dune exec project/tests/myTest.exe


Running tests in a directory
----------------------------

You can also pass a directory argument to run the tests from a sub-tree. For
instance ``dune runtest test`` will only run the tests from the ``test``
directory and any sub-directory of ``test`` recursively.

.. _inline_tests:

Inline tests
============

There are several inline tests framework available for OCaml, such as
ppx_inline_test_ and qtest_. We will use ppx_inline_test_ as an
example as at the time of writing this document it has the necessary
setup to be used with dune out of the box.

ppx_inline_test_ allows one to write tests directly inside ml files as
follows:

.. code:: ocaml

          let rec fact n = if n = 1 then 1 else n * fact (n - 1)

          let%test _ = fact 5 = 120

The file has to be preprocessed with the ppx_inline_test ppx rewriter,
so for instance the ``dune`` file might look like this:

.. code:: scheme

          (library
           (name foo)
           (preprocess (pps ppx_inline_test)))

In order to instruct dune that our library contains inline tests,
all we have to do is add an ``inline_tests`` field:

.. code:: scheme

          (library
           (name foo)
           (inline_tests)
           (preprocess (pps ppx_inline_test)))

We can now build and execute this test by running ``dune runtest``. For
instance, if we make the test fail by replacing ``120`` by ``0`` we get:

.. code:: bash

          $ dune runtest
          [...]
          File "src/fact.ml", line 3, characters 0-25: <<(fact 5) = 0>> is false.

          FAILED 1 / 1 tests

Note that in this case Dune knew how to build and run the tests
without any special configuration. This is because ppx_inline_test
defines an inline tests backend and it is used by the library. Some
other frameworks, such as qtest_ don't have any special library or ppx
rewriter. To use such a framework, you must tell dune about it
since it cannot guess it. You can do that by adding a ``backend``
field:

.. code:: scheme

          (library
           (name foo)
           (inline_tests (backend qtest.lib)))

In the example above, the name `qtest.lib` comes from the `public_name` field
in `qtest`'s own `dune` file.


Inline expectation tests
------------------------

Inline expectation tests are a special case of inline tests where you
write a bit of OCaml code that prints something followed by what you
expect this code to print. For instance, using ppx_expect_:

.. code:: ocaml

          let%expect_test _ =
            print_endline "Hello, world!";
            [%expect{|
              Hello, world!
            |}]

The test procedure consist of executing the OCaml code and replacing
the contents of the ``[%expect]`` extension point by the real
output. You then get a new file that you can compare to the original
source file. Expectation tests are a neat way to write tests as the
following test elements are clearly identified:

- the code of the test
- the test expectation
- the test outcome

You can have a look at `this blog post
<https://blog.janestreet.com/testing-with-expectations/>`_ to find out
more about expectation tests. To dune, the workflow for
expectation tests is always as follows:

- write the test with some empty expect nodes in it
- run the tests
- check the suggested correction and promote it as the original source
  file if you are happy with it

Dune makes this workflow very easy, simply add ``ppx_expect`` to
your list of ppx rewriters as follows:

.. code:: scheme

          (library
           (name foo)
           (inline_tests)
           (preprocess (pps ppx_expect)))

Then calling ``dune runtest`` will run these tests and in case of
mismatch dune will print a diff of the original source file and
the suggested correction. For instance:

.. code:: bash

          $ dune runtest
          [...]
          -src/fact.ml
          +src/fact.ml.corrected
          File "src/fact.ml", line 5, characters 0-1:
          let rec fact n = if n = 1 then 1 else n * fact (n - 1)

          let%expect_test _ =
            print_int (fact 5);
          -  [%expect]
          +  [%expect{| 120 |}]

In order to accept the correction, simply run:

.. code:: bash

          $ dune promote

You can also make dune automatically accept the correction after
running the tests by typing:

.. code:: bash

          $ dune runtest --auto-promote

Finally, some editor integration is possible to make the editor do the
promotion and make the workflow even smoother.

Running a subset of the test suite
----------------------------------


You may also run a group of tests located under a directory with:

.. code:: bash

   dune runtest mylib/tests

The above command will run all tests defined in tests and its sub-directories.

Running tests in bytecode or JavaScript
---------------------------------------

By default Dune run inline tests in native mode, except if native
compilation is not available in which case it runs them in bytecode.

You can change this setting to choose which modes tests should run
in. To do that, add a ``modes`` field to the ``inline_tests``
field.  Available modes are:

- ``byte`` for running tests in byte code
- ``native`` for running tests in native mode
- ``best`` for running tests in native mode with fallback to byte code
  if native compilation is not available
- ``js`` for running tests in JavaScript using Node.js

For instance:

.. code:: ocaml

          (library
           (name foo)
           (inline_tests (modes byte best js))
           (preprocess (pps ppx_expect)))

Specifying inline test dependencies
-----------------------------------

If your tests are reading files, you must say it to dune by adding
a ``deps`` field the ``inline_tests`` field. The argument of this
``deps`` field follows the usual :ref:`deps-field`. For instance:

.. code:: ocaml

          (library
           (name foo)
           (inline_tests (deps data.txt))
           (preprocess (pps ppx_expect)))

Passing special arguments to the test runner
--------------------------------------------

Under the hood, a test executable is built by dune. Depending on
the backend used this runner might take useful command line
arguments. You can specify such flags by using a ``flags`` field, such
as:

.. code:: ocaml

          (library
           (name foo)
           (inline_tests (flags (-foo bar)))
           (preprocess (pps ppx_expect)))

The argument of the ``flags`` field follows the :ref:`ordered-set-language`.

Using additional libraries in the test runner
---------------------------------------------

When tests are not part of the library code, it is possible that tests
require additional libraries than the library being tested. This is
the case with qtest_ as tests are written in comments. You can specify
such libraries using a ``libraries`` field, such as:

.. code:: ocaml

          (library
           (name foo)
           (inline_tests (backend qtest)
                         (libraries bar)))

Defining your own inline test backend
-------------------------------------

If you are writing a test framework, or for specific cases, you might
want to define your own inline tests backend. If your framework is
naturally implemented by a library or ppx rewriter that the user must
use when they want to write tests, then you should define this library
has a backend. Otherwise simply create an empty library with the name
you want to give for your backend.

In order to define a library as an inline tests backend, simply add an
``inline_tests.backend`` field to the library stanza. An inline tests
backend is specified by thee parameters:

1. How to create the test runner
2. How to build the test runner
3. How to run the test runner

These three parameters can be specified inside the
``inline_tests.backend`` field, which accepts the following fields:

.. code:: scheme

          (generate_runner   <action>)
          (runner_libraries (<ocaml-libraries>))
          (flags             <flags>)
          (extends          (<backends>))

For instance:

``<action>`` follows the :ref:`user-actions` specification. It
describe an action that should be executed in the directory of
libraries using this backend for their tests.  It is expected that the
action produces some OCaml code on its standard output. This code will
constitute the test runner. The action can use the following
additional variables:

- ``%{library-name}`` which is the name of the library being tested
- ``%{impl-files}`` which is the list of implementation files in the
  library, i.e. all the ``.ml`` and ``.re`` files
- ``%{intf-files}`` which is the list of interface files in the library,
  i.e. all the ``.mli`` and ``.rei`` files

The ``runner_libraries`` field specifies what OCaml libraries the test
runner uses. For instance, if the ``generate_runner`` actions
generates something like ``My_test_framework.runtests ()``, the you
should probably put ``my_test_framework`` in the ``runner_libraries``
field.

If you test runner needs specific flags, you should pass them in the
``flags`` field. You can use the ``%{library-name}`` variable in this
field.

Finally, a backend can be an extension of another backend. In this
case you must specify by in the ``extends`` field. For instance,
ppx_expect_ is an extension of ppx_inline_test_. It is possible to use
a backend with several extensions in a library, however there must be
exactly one *root backend*, i.e. exactly one backend that is not an
extension of another one.

When using a backend with extensions, the various fields are simply
concatenated. The order in which they are concatenated is unspecified,
however if a backend ``b`` extends of a backend ``a``, then ``a`` will
always come before ``b``.

Example of backend
~~~~~~~~~~~~~~~~~~

In this example, we put tests in comments of the form:

.. code:: ocaml

          (*TEST: assert (fact 5 = 120) *)

The backend for such a framework looks like this:

.. code:: lisp

          (library
           (name simple_tests)
           (inline_tests.backend
            (generate_runner (run sed "s/(\\*TEST:\\(.*\\)\\*)/let () = \\1;;/" %{impl-files}))
            ))

Now all you have to do is write ``(inline_tests ((backend
simple_tests)))`` wherever you want to write such tests. Note that
this is only an example, we do not recommend using ``sed`` in your
build as this would cause portability problems.

Custom tests
============

We said in `Running tests`_ that to run tests dune simply builds
the ``runtest`` alias. As a result, to define custom tests, you simply
need to add an action to this alias in any directory. For instance if
you have a binary ``tests.exe`` that you want to run as part of
running your testsuite, simply add this to a dune file:

.. code:: scheme

          (rule
           (alias  runtest)
           (action (run ./tests.exe)))

Hence to define an a test a pair of alias and executable stanzas are required.
To simplify this common pattern, dune provides a :ref:`tests-stanza` stanza to
define multiple tests and their aliases at once:

.. code:: scheme

   (tests (names test1 test2))

Diffing the result
------------------

It is often the case that we want to compare the output of a test to
some expected one. For that, dune offers the ``diff`` command,
which in essence is the same as running the ``diff`` tool, except that
it is more integrated in dune and especially with the ``promote``
command. For instance let's consider this test:

.. code:: scheme

          (rule
           (with-stdout-to tests.output (run ./tests.exe)))

          (rule
           (alias runtest)
           (action (diff tests.expected test.output)))

After having run ``tests.exe`` and dumping its output to ``tests.output``, dune
will compare the latter to ``tests.expected``. In case of mismatch, dune will
print a diff and then the ``dune promote`` command can be used to copy over the
generated ``test.output`` file to ``tests.expected`` in the source tree.

Alternatively, the :ref:`tests-stanza` also supports this style of tests.

.. code:: scheme

   (tests (names tests))

Where dune expects a ``tests.expected`` file to exist to infer that this is an
expect tests.

This provides a nice way of dealing with the usual *write code*,
*run*, *promote* cycle of testing. For instance:

.. code:: bash

          $ dune runtest
          [...]
          -tests.expected
          +tests.output
          File "tests.expected", line 1, characters 0-1:
          -Hello, world!
          +Good bye!
          $ dune promote
          Promoting _build/default/tests.output to tests.expected.

Note that if available, the diffing is done using the patdiff_ tool,
which displays nicer looking diffs that the standard ``diff``
tool. You can change that by passing ``--diff-command CMD`` to
dune.

Cram Tests
==========

Cram tests are expectation tests written in a shell-like syntax. They are ideal
for testing binaries. Cram tests are auto discovered from files or directories
with a ``.t`` extension, so they must be enabled manually in the
``dune-project`` file:

.. code:: scheme

   (lang dune 2.8)
   (cram enable)


File Tests
----------

To define a standalone test, we create a ``.t`` file. For example, ``foo.t``:

.. code:: bash

   Simplest possible cram test
     $ echo "testing"

This simple example demonstrates two components of cram tests:

* Comments - Anything that doesn't start with a 2 space indentation is a comment

* Commands - A command starts with 2 spaces followed by a ``$``. It is executed
  in the shell and the output is diffed against the output below. In this
  example, there's no output yet.

To run the test and promote the results:

.. code:: bash

   $ dune runtest
   $ dune promote

We now see the output of the command:

.. code:: bash

   Simplest possible cram test
     $ echo "testing"
     testing

This is the main advantage of expect tests. We don't need to write assertions
manually, instead we detect failure when the command produces a different output
than what is recorded in the test script.

For example, here's an example of how we'd test the ``wc`` utility. ``wc.t``:

.. code:: bash

   We create a test artifact called foo
     $ cat >foo <<EOF
     > foo
     > bar
     > baz
     > EOF

   After creating the fixture, we want to verify that ``wc`` gives us the right
   result:
     $ wc -l foo | awk '{ print $1 }'
     4

The above example uses the here doc syntax to pipe the subsequent lines to
``cat``. This is convenient for creating small test artifacts.

Directory Tests
---------------

In the above example we used ``cat`` to create the test artifact, but what if
there are too many artifacts to comfortably fit in test file? Or some of the
artifacts are binary? It's possible to include the artifacts as normal files or
directories provided the test is defined as a directory. The name of the test
directory must end with ``.t`` and must include a ``run.t`` as the test script.
Everything else in that directory is treated as raw data for the test. It's not
possible to define rules using ``dune`` files in such a directory.

We convert the ``wc`` test above into a directory test ``wc.t``:

.. code:: bash

   $ ls wc.t
     run.t foo.txt bar/

This defines a directory test ``wc.t`` which must include a ``run.t`` file as
the test script, with ``fool.txt`` and ``bar`` are test artifacts. We may then
access their contents in the test script ``run.t``:

.. code:: bash

   $ wc -l foo | awk '{ print $1 }'
   4
   $ wc -l $(ls bar) | awk '{ print $1 }'
   1231

Test Options
------------

When testing binaries, it's important to to specify a dependency on the binary
for two reasons:

- Dune must know to re-run the test when a dependency changes

- The dependencies must be specified to guarantee that they are visible to the
  test when running it.

We can specify dependencies using the ``deps`` field using the usual syntax:

.. code:: bash

   (cram
    (deps ../foo.exe))

This introduces a dependency on ``foo.exe`` on all cram tests in this directory.
To apply the stanza to a particular test, it's possible to use ``applies_to``
field:

.. code:: scheme

   (cram
    (applies_to * \ foo bar)
    (deps ../foo.exe))

We use the :ref:`predicate-lang` to apply this stanza to all tests in this
directory except for ``foo.t`` and ``bar.t``. The ``applies_to`` field also
accepts the special value ``:whole_subtree`` in order to apply the options to all tests
in all sub directories (recursively). This is useful to apply common options to
an entire test suite.

The ``cram`` stanza accepts the following fields:

- ``enabled_if`` - controls whether the tests are enabled
- ``alias`` - alias that can be used to run the test. In addition to the user
  alias, every test ``foo.t`` is attached to the ``@runtest`` alias and gets its
  own ``@foo`` alias to make it convenient to run individually.
- ``deps`` - dependencies of the test

A single test may be configured by more than one ``cram`` stanza. In such cases,
the values from all applicable ``cram`` stanzas are merged together to get the
final values for all the fields.

Testing an OCaml Program
------------------------

The most common testing situation involves testing an executable that is defined
in dune. For example:

.. code:: scheme

   (executable
    (name wc)
    (public_name wc))

To use this binary in the cram test, we should depend on the binary in the test:

.. code:: scheme

   (cram
    (deps %{bin:wc}))

Sandboxing
----------

Since cram tests often create intermediate artifacts, it's important that cram
tests are executed in a clean environment. This is why all cram tests are
sandboxed. To respect sandboxing, every test should specify dependency on any
artifact that might rely on using the ``deps`` field.

See :ref:`dune-action-plugin` for details about the sandboxing mechanism.

Test Output Sanitation
----------------------

In some situations, cram tests emit non portable or non deterministic output. We
recommend to sanitize such outputs using pipes. For example, we can scrub the
ocaml magic number using sed as follows:

.. code:: bash

   $ ocamlc -config | grep "cmi_magic_number:" | sed 's/Caml.*/$SPECIAL_CODE/'
   cmi_magic_number: $SPECIAL_CODE

By default, dune will scrub the some paths from the output of the tests. The
default list of paths is:

- The ``PWD`` of the test will be replaced by ``$TESTCASE_ROOT``
- The temporary directory for the current script will be replaced by ``$TMPDIR``

To add additional paths to this sanitation mechanism, it's sufficient to modify
the standard BUILD_PATH_PREFIX_MAP_ environment variable. For example:

.. code:: bash

   $ export BUILD_PATH_PREFIX_MAP="HOME=$HOME:$BUILD_PATH_PREFIX_MAP"
   $ echo $HOME
   $HOME

.. _ppx_inline_test:       https://github.com/janestreet/ppx_inline_test
.. _ppx_expect:            https://github.com/janestreet/ppx_expect
.. _qtest:                 https://github.com/vincent-hugot/qtest
.. _patdiff:               https://github.com/janestreet/patdiff
.. _cram:                  https://bitheap.org/cram/
.. _BUILD_PATH_PREFIX_MAP: https://reproducible-builds.org/specs/build-path-prefix-map/
