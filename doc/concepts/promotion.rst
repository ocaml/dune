Diffing and Promotion
=====================

You can use Diffing and Promotion flows to compare the outputs of your build in
the build directory with the source tree and/or copy the result of the rules
into your source tree to store the changes.

Diffing
=======

You can use the ``(diff <file1> <file2>)`` directive in a rule to compare
its output with the version in your source tree. It is useful when
your tests produce a file output and you want to make sure that output has
not changed.

.. TODO(diataxis)
   - howto: diffing and promotion
   - reference: diffing

``(diff <file1> <file2>)`` is very similar to ``(run diff <file1>
<file2>)``. In particular it behaves in the same way:

- When ``<file1>`` and ``<file2>`` are equal, it does nothing.
- When they are not, the differences are shown and the action fails.

However, it is different for the following reason:

- The exact command used for diff files can be configured via the
  ``--diff-command`` command line argument. Note that it's only
  called when the files are not byte equals

- By default, it will use ``patdiff`` if it is installed. ``patdiff``
  is a better diffing program. You can install it via opam with:

  .. code:: console

     $ opam install patdiff

- On Windows, both ``(diff a b)`` and ``(diff? a b)`` normalize
  end-of-line characters before comparing the files.

- Since ``(diff a b)`` is a built-in action, Dune knows that ``a``
  and ``b`` are needed, so you don't need to specify them
  explicitly as dependencies.

- You can use ``(diff? a b)`` after a command that might or might not
  produce ``b``, for cases where commands optionally produce a
  *corrected* file

- If ``<file1>`` doesn't exist, it will compare with the empty file.

- It allows promotion. See below.

Note that ``(cmp a b)`` does no end-of-line normalization and doesn't
print a diff when the files differ. ``cmp`` is meant to be used with
binary files.

Promotion
=========

Promotion relates to copying the output of a Dune rule to your source tree.
Common uses include updating rule output after a failed diff (e.g., from a
test) or committing output to source control to cut down on dependencies 
during packaging.

Promoting Test or Rule Output After Diffing
-------------------------------------------

Whenever an action ``(diff <file1> <file2>)`` or ``(diff?  <file1>
<file2>)`` fails because the two files are different, Dune allows
you to promote ``<file2>`` as ``<file1>`` if ``<file1>`` is a source
file and ``<file2>`` is a generated file.

More precisely, let's consider the following Dune file:

.. code:: dune

   (rule
    (with-stdout-to data.out (run ./test.exe)))

   (rule
    (alias   runtest)
    (action (diff data.expected data.out)))

Where ``data.expected`` is a file committed in the source
repository. You can use the following workflow to update your test:

- Update the code of your test.
- Run ``dune runtest``. The diff action will fail and a diff will
  be printed.
- Check the diff to make sure it's what you expect. This diff can be displayed
  again by running ``dune promotion diff``.
- Run ``dune promote``. This will copy the generated ``data.out``
  file to ``data.expected`` directly in the source tree.

You can also use ``dune runtest --auto-promote``, which will
automatically do the promotion.

Automatically Promoting Rule Output Into the Source Tree
--------------------------------------------------------

Dune rules support a ``(mode promote)`` directive that will automatically
copy their output into your source tree. This approach suits, for example, code
documentation generation flows where output needs to be committed to source
code control to enable easier browsing, or eliminate dependencies on a code 
generation step during opam package installation.

More information, including customising when the source is copied, can be found
in :doc:`../reference/dune/rule`.
