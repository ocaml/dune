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
