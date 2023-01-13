copy_files
----------

The ``copy_files`` and ``copy_files#`` stanzas specify that files from another
directory could be copied to the current directory, if needed.

The syntax is as follows:

.. code:: scheme

    (copy_files
     <optional-fields>
     (files <glob>))

``<glob>`` represents the set of files to copy. See the :ref:`glob <glob>` for
details.

``<optional-fields>`` are:

- ``(alias <alias-name>)`` specifies an alias to which to attach the targets.

- ``(mode <mode>)`` specifies how to handle the targets. See `modes`_ for
  details.

- ``(enabled_if <blang expression>)`` conditionally disables this stanza. The
  condition is specified using the :ref:`blang`.

The short form:

.. code:: scheme

    (copy_files <glob>)

is equivalent to:

.. code:: scheme

    (copy_files (files <glob>))

The difference between ``copy_files`` and ``copy_files#`` is the same as the
difference between the ``copy`` and ``copy#`` actions. See the
:ref:`user-actions` section for more details.
