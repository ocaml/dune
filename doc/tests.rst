.. _writing-tests:

*************************
Writing and Running Tests
*************************

Dune tries to streamline the testing story as much as possible, so
you can focus on the tests themselves and not bother with setting
up various test frameworks.

In this section, we'll explain the workflow to deal with tests in Dune. In
particular, we'll see how to run the test suite of a project, how to describe
your tests to Dune, and how to promote test results as expectation.

We distinguish three kinds of tests:

* Inline tests - written directly inside the ``.ml`` files of a library

* Custom tests - run an executable, possibly followed by an action such as
  diffing the produced output.

* Cram tests - expect tests written in Cram_ style.


Running Tests
=============

Whatever the tests of a project are, the usual way to run tests with Dune is to
call ``dune runtest`` from the shell (or the command alias ``dune test``). This
will run all the tests defined in the current directory and any subdirectory
recursively.

Note that in any case, ``dune runtest`` is simply shorthand for building the
``runtest`` alias, so you can always ask Dune to run the tests in conjunction
with other targets by passing ``@runtest`` to ``dune build``. For instance:

.. code:: bash

   $ dune build @install @runtest
   $ dune build @install @test/runtest


Running a Single Test
---------------------

If you would only like to run a single test for your project, you may use ``dune
exec`` to run the test executable (for the sake of this example,
``project/tests/myTest.ml``):

.. code:: bash

   dune exec project/tests/myTest.exe

To run :ref:`cram-tests`, you can use the alias that is created for the test.
The name of the alias corresponds to the name of the test without the ``.t``
extension. For directory tests, this is the name of the directory without the
``.t`` extension. Assuming a ``cram-test.t`` or ``cram-test.t/run.t`` file
exists, it can be run with:

.. code:: bash

   $ dune build @cram-test


Running Tests in a Directory
----------------------------

You can also pass a directory argument to run the tests from a subtree. For
instance, ``dune runtest test`` will only run the tests from the ``test``
directory and any subdirectory of ``test`` recursively.

.. _inline_tests:


Inline Tests
============

There are several inline tests frameworks available for OCaml, such as
ppx_inline_test_ and qtest_. We will use ppx_inline_test_ as an
example because it has the necessary setup to be used with Dune out of the box.

ppx_inline_test_ allows one to write tests directly inside ``.ml`` files as
follows:


.. code:: ocaml


   let rec fact n = if n = 1 then 1 else n * fact (n - 1)

   let%test _ = fact 5 = 120

The file must be preprocessed with the ``ppx_inline_test`` PPX rewriter,
so for instance the ``dune`` file might look like this:

.. code:: dune

   (library
    (name foo)
    (preprocess (pps ppx_inline_test)))

In order to tell Dune that our library contains inline tests, 
we have to add an ``inline_tests`` field:

.. code:: dune

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
without any special configuration. This is because ``ppx_inline_test``
defines an inline tests backend that's used by the library. Some
other frameworks, such as qtest_, don't have any special library or PPX
rewriter. To use such a framework, you must tell Dune about it,
as it cannot guess. You can do that by adding a ``backend``
field:

.. code:: dune

   (library
    (name foo)
    (inline_tests (backend qtest.lib)))

In the example above, the name `qtest.lib` comes from the `public_name` field
in `qtest`'s own `dune` file.

Note that using ``ppx_inline_test`` requires that the opam package
``ppx_inline_test`` be installed in your switch. If you use ``ppx_inline_test``
in a package then that package must `unconditionally` depend on
``ppx_inline_test`` (ie. ``ppx_inline_test`` can't be a ``with-test``
dependency).

Inline Expectation Tests
------------------------

Inline expectation tests are a special case of inline tests where written OCaml code 
prints something followed by what you expect this code to print. 
For instance, using ppx_expect_:

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

- The code of the test
- The test expectation
- The test outcome

You can have a look at `this blog post
<https://blog.janestreet.com/testing-with-expectations/>`_ to find out
more about expectation tests. To Dune, the workflow for
expectation tests is always as follows:

- Write the test with some empty expect nodes in it
- Run the tests
- Check the suggested correction and promote it as the original source
  file if you are happy with it

Dune makes this workflow very easy. Simply add ``ppx_expect`` to
your list of PPX rewriters as follows:

.. code:: dune

   (library
    (name foo)
    (inline_tests)
    (preprocess (pps ppx_expect)))

Then calling ``dune runtest`` will run these tests, and in case of
mismatch, Dune will print a diff of the original source file and
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

You can also make Dune automatically accept the correction after
running the tests by typing:

.. code:: bash

   $ dune runtest --auto-promote

Finally, some editor integration can make the editor do the
promotion, which in turn makes the workflow even smoother.


Running a Subset of the Test Suite
----------------------------------

You may also run a group of tests located under a directory with:

.. code:: bash

   dune runtest mylib/tests

The above command will run all tests defined in tests and its subdirectories.


Running Tests in Bytecode or JavaScript
---------------------------------------

By default, Dune runs inline tests in native mode, unless native
compilation isn't available. In which case, it runs them in bytecode.
You can change this setting to choose the modes that tests should run
in. To do this, add a ``modes`` field to the ``inline_tests``
field. Available modes are:

- ``byte`` for running tests in byte code
- ``native`` for running tests in native mode
- ``best`` for running tests in native mode with fallback to byte code,
  if native compilation is not available
- ``js`` for running tests in JavaScript using Node.js

For instance:

.. code:: ocaml

   (library
    (name foo)
    (inline_tests (modes byte best js))
    (preprocess (pps ppx_expect)))


Specifying Inline Test Dependencies
-----------------------------------

If your tests are reading files, you must tell Dune by adding
a ``deps`` field the ``inline_tests`` field. The argument of this
``deps`` field follows the usual :doc:`concepts/dependency-spec`. For instance:

.. code:: ocaml

   (library
    (name foo)
    (inline_tests (deps data.txt))
    (preprocess (pps ppx_expect)))


Passing Special Arguments to the Test Runner
--------------------------------------------

Under the hood, a test executable is built by Dune. Depending on
the backend used, this runner might take useful command line
arguments. You can specify such flags by using a ``flags`` field, such
as:

.. code:: ocaml

   (library
    (name foo)
    (inline_tests (flags (-foo bar)))
    (preprocess (pps ppx_expect)))

The argument of the ``flags`` field follows the
:doc:`reference/ordered-set-language`.


Passing Special Arguments to the Test Executable
------------------------------------------------

To control how the test executable is built, it's possible to customize a subset
of compilation options for an executable using the ``executable`` field. Dune
gives you this ability by simply specifying command line arguments as flags.
You can specify such flags by using ``flags`` field. For instance:

.. code:: ocaml

   (library
    (name foo)
    (inline_tests
     (flags (-foo bar)
     (executable
      (flags (-foo bar))))
     (preprocess (pps ppx_expect))))

The argument of the ``flags`` field follows the :doc:`reference/ordered-set-language`.

Using Additional Libraries in the Test Runner
---------------------------------------------

When tests are not part of the library code, it's possible that tests
require additional libraries than the library being tested. This is
the case with qtest_, as tests are written in comments. You can specify
such libraries using a ``libraries`` field, such as:

.. code:: ocaml

   (library
    (name foo)
    (inline_tests
     (backend qtest)
     (libraries bar)))


Changing the Flags of the Linking Step of the Test Runner
---------------------------------------------------------

You can use the ``link_flags`` field to change the linker flags
passed to ``ocamlopt`` when building the test runner. By default, the
linking flags are ``-linkall``. You probably want to keep
``-linkall`` as one of the new list of flags (unless you know what you
are doing), forcing the linker to load your test module, since the test
runner doesn't depend on anything itself. This field supports
``(:include ...)`` forms.

.. code:: dune

   (library
    (name foo)
    (inline_tests
     (executable
      (link_flags -linkall -noautolink -cclib -Wl,-Bstatic -cclib -lm)))
    (preprocess (pps ppx_expect)))


Defining Your Own Inline Test Backend
-------------------------------------

If you are writing a test framework (or for other specific cases), you might
want to define your own inline tests backend. If your framework is
naturally implemented by a library or PPX rewriter that's necessary to write tests, 
you should define this library as a backend. Otherwise simply create an 
empty library with your chosen backend's name.

In order to define a library as an inline tests backend, simply add an
``inline_tests.backend`` field to the library stanza. An inline tests
backend is specified by four parameters:

1. How to create the test runner
2. How to build the test runner
3. How to run the test runner
4. Optionally how to run the test runner to list partitions

These four parameters can be specified inside the
``inline_tests.backend`` field, which accepts the following fields:

.. code:: dune

   (generate_runner       <action>)
   (runner_libraries     (<ocaml-libraries>))
   (flags                 <flags>)
   (list_partitions_flags <flags>)
   (extends              (<backends>))

For instance:

``<action>`` follows the :doc:`reference/actions` specification. It describes an
action that should be executed in the library's directory using this backend
for their tests. It's expected that the action will produce some OCaml code on
its standard output. This code will constitute the test runner. The action can
use the following additional variables:

- ``%{library-name}`` --- the name of the library being tested
- ``%{impl-files}`` --- the list of implementation files in the
  library, i.e., all the ``.ml`` and ``.re`` files
- ``%{intf-files}`` --- the list of interface files in the library,
  i.e., all the ``.mli`` and ``.rei`` files

The ``runner_libraries`` field specifies what OCaml libraries the test
runner uses. For instance, if the ``generate_runner`` actions
generates something like ``My_test_framework.runtests ()``, then you
should probably put ``my_test_framework`` in the ``runner_libraries``
field.

If your test runner needs specific flags, you should pass them in the
``flags`` field. You can use the ``%{library-name}`` variable in this
field.

If your test runner supports test partitions, you should pass the
flags necessary for listing partitions in the
``list_partitions_flags`` field. In such scenario, the ``flags`` field
will also accepts a ``%{partition}`` variable.

Finally, a backend can be an extension of another backend. In this
case, you must specify this in the ``extends`` field. For instance,
ppx_expect_ is an extension of ppx_inline_test_. It's possible to use
a backend with several extensions in a library; however, there must be
exactly one *root backend*, i.e., exactly one backend that isn't an
extension of another one.

When using a backend with extensions, the various fields are simply
concatenated. The order in which they are concatenated is unspecified;
however, if a backend ``b`` extends a backend ``a``, then ``a`` will
always come before ``b``.


Example of Backend
~~~~~~~~~~~~~~~~~~

In this example, we put tests in comments of the form:

.. code:: ocaml

   (*TEST: assert (fact 5 = 120) *)

The backend for such a framework looks like this:

.. code:: dune

   (library
    (name simple_tests)
    (inline_tests.backend
     (generate_runner (run sed "s/(\\*TEST:\\(.*\\)\\*)/let () = \\1;;/" %{impl-files}))))

Now all you have to do is write ``(inline_tests ((backend
simple_tests)))`` wherever you want to write such tests. Note that
this is only an example. We don't recommend using ``sed`` in your
build, as this would cause portability problems.


Custom Tests
============

We said in `Running tests`_ that to run tests, Dune simply builds
the ``runtest`` alias. As a result, you simply need to add an action 
to this alias in any directory in order to define custom tests. For instance, if
you have a binary ``tests.exe`` that you want to run as part of
running your test suite, simply add this to a ``dune`` file:

.. code:: dune

   (rule
    (alias  runtest)
    (action (run ./tests.exe)))

Hence to define a test, a pair of alias and executable stanzas are required.
To simplify this common pattern, Dune provides a :ref:`tests-stanza` stanza to
define multiple tests and their aliases at once:

.. code:: dune

   (tests (names test1 test2))


Diffing the Result
------------------

It's often the case that we want to compare the actual output of a test to
an expected one. For that, Dune offers the ``diff`` command,
which in essence is the same as running the ``diff`` tool, except that
it's more integrated in Dune, especially with the ``promote``
command. For instance, let's consider this test:

.. code:: dune

   (rule
   (with-stdout-to tests.output (run ./tests.exe)))

   (rule
    (alias runtest)
    (action (diff tests.expected tests.output)))

After having run ``tests.exe`` and dumping its output to ``tests.output``, Dune
will compare the latter to ``tests.expected``. In case of mismatch, Dune will
print a diff and then the ``dune promote`` command can be used to copy over the
generated ``test.output`` file to ``tests.expected`` in the source tree.

Alternatively, the :ref:`tests-stanza` also supports this style of tests.

.. code:: dune

   (tests (names tests))

Dune expects the existence of a ``tests.expected`` file to infer that this is an
expected test.

This provides a nice way of dealing with the usual *write code*,
*run*, and *promote* cycle of testing. For instance:

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
which displays nicer looking diffs than the standard ``diff``
tool. You can change that by passing ``--diff-command CMD`` to
Dune.

.. _cram-tests:


Cram Tests
==========

Cram tests are expectation tests written in a shell-like syntax. They are ideal
for testing binaries. Cram tests are automatically discovered from files or directories
with a ``.t`` extension. By default, this has been enabled since Dune 3.0. For
older versions, it must be manually enabled in the ``dune-project`` file:

.. code:: dune

   (lang dune 2.7)
   (cram enable)


File Tests
----------

To define a standalone test, we create a ``.t`` file. For example, ``foo.t``:

.. code:: bash

   Simplest possible Cram test
     $ echo "testing"

This simple example demonstrates two components of Cram tests:

* Comments - Anything that doesn't start with a 2 space indentation is a comment
* Commands - A command starts with 2 spaces followed by a ``$``. It's executed
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
manually; instead we detect failure when the command produces a different output
than what is recorded in the test script.

For example, here's an example of how we'd test the ``wc`` utility. ``wc.t``:

.. code:: bash

   We create a test artifact called "foo"
     $ cat >foo <<EOF
     > foo
     > bar
     > baz
     > EOF

   After creating the fixture, we want to verify that ``wc`` gives us the right
   result:
     $ wc -l foo | awk '{ print $1 }'
     4

The above example uses the doc syntax, piping the subsequent lines to
``cat``. This is convenient for creating small test artifacts.


Directory Tests
---------------

In the above example we used ``cat`` to create the test artifact, but what if
there are too many artifacts to comfortably fit in test file? Or some of the
artifacts are binary? 

It's possible to include the artifacts as normal files or
directories, provided the test is defined as a directory. The name of the test
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

   Testing wc:
     $ wc -l foo | awk '{ print $1 }'
     4
     $ wc -l $(ls bar) | awk '{ print $1 }'
     1231


Test Options
------------

When testing binaries, it's important to to specify a dependency on the binary
for two reasons:

- Dune must know to re-run the test when a dependency changes
- The dependencies must be specified to guarantee that they're visible to the
  test when running it.

We can specify dependencies using the ``deps`` field using the usual syntax:

.. code:: dune

   (cram
    (deps ../foo.exe))
    
This introduces a dependency on ``foo.exe`` on all Cram tests in this directory.
To apply the stanza to a particular test, it's possible to use ``applies_to``
field:

.. code:: dune

   (cram
    (applies_to * \ foo bar)
    (deps ../foo.exe))

We use the :doc:`reference/predicate-language` to apply this stanza to all tests
in this directory, except for ``foo.t`` and ``bar.t``. The ``applies_to`` field
also accepts the special value ``:whole_subtree`` in order to apply the options
to all tests in all subdirectories (recursively). This is useful to apply
common options to an entire test suite.

The ``cram`` stanza accepts the following fields:

- ``enabled_if`` - controls whether the tests are enabled
- ``alias`` - alias that can be used to run the test. In addition to the user
  alias, every test ``foo.t`` is attached to the ``@runtest`` alias and gets its
  own ``@foo`` alias to make it convenient to run individually.
- ``(locks (<lock-names>))`` specify that the tests must be run while
  holding the following locks. See :doc:`concepts/locks` for more details.
- ``deps`` - dependencies of the test
- ``(package <package-name>)`` - attach the tests selected by this stanza to the
  specified package
- ``(shell <shell-prog> <args>..)`` - (available since Dune 3.12) specify the shell to be
  used when executing the commands in Cram test files. ``<shell-prog>`` can now
  be one of the following values:

  - ``:system`` - this stipulates that the system shell (that is, what you get
    with ``/usr/bin/env sh``) should be used. This is the default when this
    field does not present.

  - ``<dep>`` - ``<dep>`` should specify the path to an executable, which will
    be used as the shell program. :doc:`concepts/variables` will be expanded.

A single test may be configured by more than one ``cram`` stanza. In such cases,
the values from all applicable ``cram`` stanzas are merged together to get the
final values for all the fields.


Testing an OCaml Program
------------------------

The most common testing situation involves testing an executable that is defined
in Dune. For example:

.. code:: dune

   (executable
    (name wc)
    (public_name wc))

To use this binary in the Cram test, we should depend on the binary in the test:

.. code::

   (cram
    (deps %{bin:wc}))


Sandboxing
----------

Since Cram tests often create intermediate artifacts, it's important that Cram
tests are executed in a clean environment. This is why all Cram tests are
sandboxed. To respect sandboxing, every test should specify dependency on any
artifact that might rely on using the ``deps`` field.

See :doc:`concepts/sandboxing` for details about the sandboxing mechanism.


Test Output Sanitation
----------------------

In some situations, Cram tests emit non portable or non-deterministic output. We
recommend sanitising such outputs using pipes. For example, we can scrub the
OCaml magic number using ``sed`` as follows:

.. code:: bash

   $ ocamlc -config | grep "cmi_magic_number:" | sed 's/Caml.*/$SPECIAL_CODE/'
   cmi_magic_number: $SPECIAL_CODE

By default, Dune will scrub some paths from the output of the tests. The
default list of paths is:

- The ``PWD`` of the test will be replaced by ``$TESTCASE_ROOT``
- The temporary directory for the current script will be replaced by ``$TMPDIR``

To add additional paths to this sanitation mechanism, it's sufficient to modify
the standard BUILD_PATH_PREFIX_MAP_ environment variable. For example:

.. code:: bash

   $ export BUILD_PATH_PREFIX_MAP="HOME=$HOME:$BUILD_PATH_PREFIX_MAP"
   $ echo $HOME
   $HOME

Note: Unlike Dune's version of Cram, the original specification for Cram
supports regular expression and glob filtering for matching output. We chose
not to implement this feature because it breaks the test, diff, and accept cycle.
With regex or glob matching, the output must now be manually inspected and
possibly updated. We consider the postprocessing approach described here as
superior and will not introduce output matchers.

.. _ppx_inline_test:       https://github.com/janestreet/ppx_inline_test
.. _ppx_expect:            https://github.com/janestreet/ppx_expect
.. _qtest:                 https://github.com/vincent-hugot/qtest
.. _patdiff:               https://github.com/janestreet/patdiff
.. _cram:                  https://bitheap.org/cram/
.. _BUILD_PATH_PREFIX_MAP: https://reproducible-builds.org/specs/build-path-prefix-map/
