test
----

The ``test`` stanza is the singular form of ``tests``. The only difference is
that it's of the form:

.. code:: scheme

   (test
    (name foo)
    <optional fields>)

The ``name`` field is singular, and the same optional fields are supported.

.. _tests-stanza:

tests
-----

The ``tests`` stanza allows one to easily define multiple tests. For example, we
can define two tests at once with:

.. code:: scheme

   (tests
    (names mytest expect_test)
    <optional fields>)

This defines an executable named ``mytest.exe`` that will be executed as part of
the ``runtest`` alias. If the directory also contains an
``expect_test.expected`` file, then ``expect_test`` will be used to define an
expect test. That is, the test will be executed and its output will be compared
to ``expect_test.expected``.

The optional fields supported are a subset of the alias and executables fields.
In particular, all fields except for ``public_names`` are supported from the
:ref:`executables stanza <shared-exe-fields>`. Alias fields apart from ``name``
are allowed.

By default, the test binaries are run without options.  The ``action`` field can
override the test binary invocation, i.e., if you're using Alcotest and wish to
see all the test failures on the standard output. When running Dune ``runtest``
you can use the following stanza:

.. code:: lisp

   (tests
    (names mytest)
    (libraries alcotest mylib)
    (action (run %{test} -e)))

Starting from Dune 2.9, it's possible to automatically generate empty interface
files for test executables. See `executables_implicit_empty_intf`_.
