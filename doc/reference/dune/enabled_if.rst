enabled_if
----------

.. versionadded:: 3.26

The ``enabled_if`` stanza conditionally includes a group of stanzas in a
``dune`` file:

.. code:: dune

   (enabled_if (= %{profile} release)
    (rule (write-file release-info.txt "release"))
    (alias
     (name release-info)
     (deps release-info.txt)))

Dune evaluates the Boolean expression separately in each build context. The
expression can use ``%{profile}`` and ``%{context_name}``. If it evaluates to
``false``, Dune does not interpret the body or run any rule plugins in it.
Enabled bodies can themselves contain ``enabled_if`` stanzas.

Stanzas that are processed before the build context is known, such as
``dirs``, ``files``, ``subdir``, and ``include``, cannot appear inside
``enabled_if``. To conditionally define rules in a subdirectory, put
``enabled_if`` *inside* the ``subdir`` stanza.
