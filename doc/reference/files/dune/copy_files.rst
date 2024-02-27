############
 copy_files
############

The ``copy_files`` and ``copy_files#`` stanzas specify that files from
another directory could be copied to the current directory, if needed.

The syntax is as follows:

.. code:: dune

   (copy_files
    <optional-fields>
    (files <glob>))

``<glob>`` represents the set of files to copy. See the :ref:`glob
<glob>` for details.

``<optional-fields>`` are:

-  ``(alias <alias-name>)`` specifies an alias to which to attach the
   targets.

-  ``(mode <mode>)`` specifies how to handle the targets. See
   :ref:`modes` for details.

-  ``(enabled_if <blang expression>)`` conditionally disables this
   stanza. The condition is specified using the
   :doc:`/reference/boolean-language`.

-  ``(only_sources <blang expression>)`` specifies that the glob in
   ``files`` gets applied over the source tree, and not the build tree.

The short form:

.. code:: dune

   (copy_files <glob>)

is equivalent to:

.. code:: dune

   (copy_files (files <glob>))

The difference between ``copy_files`` and ``copy_files#`` is the same as
the difference between the ``copy`` and ``copy#`` actions. See
:doc:`/reference/actions/index` section for more details.
