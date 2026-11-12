Cram
----

.. describe:: (cram ...)

   .. versionadded:: 2.7

   Configure Cram tests in the current directory (and subdirectories).

   A single test may be configured by more than one ``cram`` stanza. In such
   cases, the values from all applicable ``cram`` stanzas are merged together
   to get the final values for all the fields.

   .. seealso:: :doc:`/reference/cram`

   .. describe:: (deps <dep-spec>)

      Specify the dependencies of the test. Defaults to no dependencies.

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

      Starting with Dune 3.25, test names include the ``.t`` suffix. In earlier
      versions, the suffix is omitted. The following will apply the stanza to
      all tests in this directory except for ``foo.t`` and ``bar.t``:

      .. code:: dune

         (cram
          (applies_to * \ foo.t bar.t)
          (deps ../foo.exe))

      .. seealso:: :doc:`/reference/predicate-language`

   .. describe:: (enabled_if <blang>)

      Control whether the tests are included in the ``runtest`` alias and other
      aliases specified by the ``alias`` field. When ``enabled_if`` evaluates to
      ``false``, the test is excluded from these aliases but can still be run
      explicitly via its own alias (e.g., ``dune build @foo.t`` for ``foo.t``
      starting with Dune 3.25).

      The default is ``true``. If several stanzas apply, all their conditions
      must hold.

      .. seealso:: :doc:`/reference/boolean-language`, :doc:`/concepts/variables`

   .. describe:: (alias <name>)

      Alias that can be used to run the test. In addition to the user alias,
      every test ``foo.t`` is attached to the :doc:`/reference/aliases/runtest`
      alias and gets its own alias to make it convenient to run individually.
      Starting with Dune 3.25, this alias is ``@foo.t``; in earlier versions it
      is ``@foo``. By default, no additional alias is added.

   .. describe:: (locks <lock-names>)

      .. versionadded:: 2.9

      Specify that the tests must be run while holding the following locks.
      Defaults to no locks.

      .. seealso:: :doc:`/concepts/locks`

   .. describe:: (package <name>)

      .. versionadded:: 2.8

      Attach the tests selected by this stanza to the specified package.
      This association is used by ``-p`` and ``--only-packages`` to filter
      tests. By default, no explicit association is added.

   .. describe:: (runtest_alias <blang>)

      .. versionadded:: 3.12

      When set to ``false``, do not add the tests to the ``runtest`` alias.
      The default is to add every Cram test to ``runtest``, but this is not
      always desired.

      Since language version 3.24, this field accepts a
      :doc:`Boolean expression </reference/boolean-language>`; earlier
      versions accept only ``true`` or ``false``. Other aliases are unaffected.
      At most one applicable stanza may set this field.

   .. describe:: (timeout <float>)

      .. versionadded:: 3.20

      Specify a time limit (in seconds) for each individual Cram test.

      If a test takes longer than the specified timeout, Dune will terminate it
      and report a timeout error. This can be useful to catch tests that hang
      or take unexpectedly long.

      The timeout is a floating-point number (e.g., `1.5` for 1.5 seconds).
      The timeout must be nonnegative. By default, there is no time limit.

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
      handled. The default behavior is to ``ignore`` them. Setting ``error``
      will make the test runner reject such conflicts and refuse to run the
      test.

   .. describe:: (setup_scripts <files-list>)

      .. versionadded:: 3.21

      Determines the list of shell files that will be sourced before the
      execution of every cram test this stanza is applied to. A single script
      will not be sourced more than once, no matter how many times it is listed
      in this field. The order of evaluation for these scripts is deterministic
      but is left undefined, so it is not recommended that these scripts have
      side effects.

      Defaults to no setup scripts. Relative paths are resolved from the
      stanza's directory.

   .. describe:: (shell <sh|bash>)

      .. versionadded:: 3.22

      Determines the shell executable to use to execute the cram script. By
      default, cram scripts will execute with ``sh``.
