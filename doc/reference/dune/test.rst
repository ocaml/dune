test
----

The ``test`` stanza is the singular form of ``tests``. The only difference is
that it's of the form:

.. code:: dune

   (test
    (name foo)
    <optional fields>)

The ``name`` field is singular, and the same optional fields are supported.

.. _tests-stanza:

tests
-----

The ``tests`` stanza allows one to easily define multiple tests. For example, we
can define two tests at once with:

.. code:: dune

   (tests
    (names mytest expect_test)
    <optional fields>)

This defines an executable named ``mytest.exe``. These tests can be run by
building the aliases ``runtest-mytest`` and ``runtest-expect_test``
respectively. They will also be added to the ``runtest`` alias.
The per-test aliases are available since Dune 3.20.

If the directory also contains an ``expect_test.expected`` file, then
``expect_test`` will be used to define an expect test. That is, the test will be
executed and its standard output will be compared to ``expect_test.expected``.
Mismatches can be accepted through :doc:`promotion </concepts/promotion>`.

The optional fields supported are a subset of the alias and executables fields.
In particular, compilation fields from the
:ref:`executables stanza <shared-exe-fields>` are supported, along with
``link_flags`` and ``forbidden_libraries``. Executable-only fields such as
``public_name``, ``public_names``, ``link_deps``, ``optional`` and ``promote``
are not supported. Alias fields apart from ``name`` are allowed.

Each name identifies an executable's main module. By default, all modules in
the directory are available; use ``modules`` to restrict their selection.

Additional optional fields are:

.. describe:: (deps <deps>)

   Specifies dependencies needed to run the tests (default: none). See
   :doc:`/concepts/dependency-spec`.

.. describe:: (locks <lock-names>)

   Specifies locks held while running the tests (default: none). See
   :doc:`/concepts/locks`.

.. describe:: (package <name>)

   Associates the tests with a package for filtering by
   ``-p`` or ``--only-packages`` (default: no explicit association).

.. describe:: (modes <modes>)

   Selects :doc:`executable` linking modes. The default is
   ``exe``; before language version 3.0, it was ``byte exe``. Since 3.0, each
   selected runnable mode is tested; earlier versions run only ``exe``.
   JavaScript and Wasm tests use ``runtest`` by default. Their alias can be
   overridden by ``runtest_alias`` in the corresponding :doc:`env` settings.

.. describe:: (enabled_if <blang>)

   .. versionadded:: 1.4

   Controls whether the tests are run (default: ``true``), without disabling
   compilation. When false, the per-test aliases are not generated.

.. describe:: (build_if <blang>)

   .. versionadded:: 3.9

   Controls whether the tests are built and run (default: ``true``).

.. describe:: (action <action>)

   .. versionadded:: 1.2

   By default, the test binaries are run without options.  The ``action`` field
   can override the test binary invocation, i.e., if you're using Alcotest and
   wish to see all the test failures on the standard output. When running Dune
   ``runtest`` you can use the following stanza:

   .. code:: dune

      (tests
       (names mytest)
       (libraries alcotest mylib)
       (action (run %{test} -e)))

   ``%{test}`` expands to the test executable's path.

The test action runs in the stanza's build directory and fails if it exits with
a nonzero status. Starting with language version 3.22, it runs in a
:doc:`sandbox </concepts/sandboxing>`.

Starting from Dune 2.9, it's possible to automatically generate empty interface
files for test executables. See
:doc:`/reference/dune-project/executables_implicit_empty_intf`.
