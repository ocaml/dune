Cram Tests
==========

Syntax
------

Cram tests are interpreted line by line, depending on the first characters of
each line:

- If a line starts with two spaces, it is part of a command or its output.

  - If the next two characters are a dollar sign ``$`` and a space character,
    the rest is a command.

  - If the next two characters are a greater-than sign ``>`` and a space
    character, the rest is the continuation of a command.

  - Otherwise (line starts with two spaces and something else), this is the
    expected output of the command just before.

- Otherwise (line does not start with two spaces), this is a comment.

Ignoring comments, a Cram test is composed of a list of commands (1 line), with
optional continuation lines, and with optional output lines.

.. code:: cram

   Plain paragraphs that do not start with spaces are ignored by Cram tests.
   They are often used to introduce commands. For example, this is a command:

     $ touch this-file.txt

   The above is the simplest command; it has no continuation lines and no output.
   Some commands have an expected output:

     $ ls
     this-file.txt

   There can be several output lines if the command is expected to print
   several lines.
   Also, note that if a command has no output, the next one can come in the
   next line.

     $ touch other-file.txt
     $ ls
     other-file.txt
     this-file.txt

   Continuation lines are used when a command fits on several lines. This can
   happen in all the cases where pressing Enter would not run the command. For
   example, when passing a backslash character to escape the line ending.
   In that case, all the continuation lines are grouped together as a single
   command.
   This syntax mimics the PS2 prompt in shells - the ">" character is not
   passed to the command.

     $ echo \
     >   a \
     >   b \
     >   d \
     >   c
     a b c d

Semantics
---------

Execution and Promotion
^^^^^^^^^^^^^^^^^^^^^^^

When a Cram test is executed, the commands it contains are executed, and a
corrected file is created where the outputs of the commands are inserted after
each command. This corrected file is then offered for
:doc:`promotion <../concepts/promotion>` by Dune.

Concretely, this means that ``dune runtest`` will display the difference
between the current contents of the Cram test, and the output of the latest
run. This diff can be applied by running ``dune promote``, as usual.

.. code:: diff

   $ touch changed-name.txt
   $ ls
  -other-file.txt
  +changed-name.txt
   this-file.txt

File and Directory Tests
^^^^^^^^^^^^^^^^^^^^^^^^

There are two types of Cram tests: file tests and directory tests. File tests
are files with a ``.t`` extension. Directory tests are files named ``run.t``
within a directory with a name that ends with ``.t``.

A Cram test begins its execution in a temporary directory where its
dependencies (as listed in the corresponding :ref:`cram stanzas <cram-stanza>`,
if any) are available. In the case of a directory test, the contents of the
directory are also available.
