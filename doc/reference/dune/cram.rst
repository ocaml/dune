Cram
----

.. describe:: (cram ...)

   Configure Cram tests in the current directory (and subdirectories).

   A single test may be configured by more than one ``cram`` stanza. In such
   cases, the values from all applicable ``cram`` stanzas are merged together
   to get the final values for all the fields.

   .. seealso:: :doc:`/reference/cram`

   .. describe:: (deps <dep-spec>)

      Specify the dependencies of the test.

      When testing binaries, it's important to to specify a dependency on the
      binary for two reasons:

      - Dune must know to re-run the test when a dependency changes
      - The dependencies must be specified to guarantee that they're visible to
        the test when running it.

      The following introduces a dependency on ``foo.exe`` on all Cram tests in
      this directory:

      .. code:: dune

         (cram
          (deps ../foo.exe))

      .. seealso:: :doc:`/concepts/dependency-spec`.

   .. describe:: (applies_to <predicate-lang>)

      Specify the scope of this ``cram`` stanza. By default it applies to all the
      Cram tests in the current directory. The special ``:whole_subtree`` value
      will apply the options to all tests in all subdirectories (recursively).
      This is useful to apply common options to an entire test suite.

      The following will apply the stanza to all tests in this directory,
      except for ``foo.t`` and ``bar.t``:

      .. code:: dune

         (cram
          (applies_to * \ foo bar)
          (deps ../foo.exe))

      .. seealso:: :doc:`/reference/predicate-language`

   .. describe:: (enabled_if <blang>)

      Control whether the tests are enabled.

      .. seealso:: :doc:`/reference/boolean-language`, :doc:`/concepts/variables`

   .. describe:: (alias <name>)

      Alias that can be used to run the test. In addition to the user alias,
      every test ``foo.t`` is attached to the :doc:`/reference/aliases/runtest`
      alias and gets its own ``@foo`` alias to make it convenient to run
      individually.

   .. describe:: (locks <lock-names>)

      Specify that the tests must be run while holding the following locks.

      .. seealso:: :doc:`/concepts/locks`

   .. describe:: (package <name>)

      Attach the tests selected by this stanza to the specified package.

   .. describe:: (runtest_alias <true|false>)

      .. versionadded:: 3.12

      When set to ``false``, do not add the tests to the ``runtest`` alias.
      The default is to add every Cram test to ``runtest``, but this is not
      always desired.

   .. describe:: (timeout <float>)

      .. versionadded:: 3.20

      Specify a time limit (in seconds) for each individual Cram test.

      If a test takes longer than the specified timeout, Dune will terminate it
      and report a timeout error. This can be useful to catch tests that hang
      or take unexpectedly long.

      The timeout is a floating-point number (e.g., `1.5` for 1.5 seconds).
      Zero or negative values cause immediate failure when running the cram
      test.

      If multiple ``cram`` stanzas apply to the same test, the **lowest** of
      all specified timeouts is used.

      This field is typically used to guard against unresponsive or
      non-terminating test cases.

      Example:

      .. code:: dune

         (cram
          (timeout 2.5))

      This limits each selected test to at most 2.5 seconds of execution time.

   .. describe:: (conflict_markers <ignore|error>)

      .. versionadded:: 3.21

      Determines how conflict markers inserted by version control systems are
      inserted. The default behavior is to ``ignore`` them. Setting ``error``
      will make the test runner reject such conflicts and refuse to run the
      test.
