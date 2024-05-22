Cram Tests
==========

Synopsis
--------

Cram tests are integrations tests that describe a shell session. These tests
contain commands and expected outputs. When executed, the commands are executed
and the actual output is compared to the expected output.

Here is an example showing how ``echo``, ``cat``, and ``rm`` interact.

.. code:: cram

   Create a file:

     $ echo contents > data.txt

   Display it:

     $ cat data.txt
     contents

   Remove it:

     $ rm data.txt

   Try to remove it again:

     $ rm data.txt
     rm: cannot remove 'data.txt': No such file or directory
     [1]

The syntax mimics a shell session: there are comments and shell commands with
their output.

Examples
--------

Simple Commands
^^^^^^^^^^^^^^^

This is the simplest test case: it executes the command ``touch
this-file.txt`` and expects that the command has no output.

.. code:: console

   $ touch this-file.txt

Output
^^^^^^

This executes ``ls`` and expects it to display ``this-file.txt``:

.. code:: console

   $ ls
   this-file.txt

There can be several output lines if the command is expected to print several
lines.
Also, note that if a command has no output, the next one can come in the next
line.

.. code:: console

   $ touch other-file.txt
   $ ls
   other-file.txt
   this-file.txt

Comments
^^^^^^^^

Lines that are not indented are ignored. These act as comments.

.. code:: cram

   "touch" will create an empty file:

     $ touch data.txt

   Printing it will do nothing:

     $ cat data.txt

Continuation Lines
^^^^^^^^^^^^^^^^^^

Continuation lines are used when a command fits on several lines. This can
happen in all the cases where pressing Enter would not run the command. For
example, when passing a backslash character to escape the line ending. In that
case, all the continuation lines are grouped together as a single command.

This syntax mimics the PS2 prompt in shells - the ">" character is not passed
to the command.

.. code:: console

   $ echo \
   >   a \
   >   b \
   >   d \
   >   c
   a b c d

This is often used with shell "heredocs" to create files:

.. code:: console

   $ cat > file.txt << EOF
   > Everything
   > here will
   > written to
   > the file
   > EOF

   $ cat file.txt
   Everything
   here will
   written to
   the file

Exit Codes
^^^^^^^^^^

When a command exits with a nonzero exit code, it is displayed between square
brackets after its output:

.. code:: console

   $ false
   [1]

   $ echo hello; false
   hello
   [1]

Syntax Details
--------------

Cram tests are parsed line by line, depending on the first characters of
each line:

- If a line starts with ``␣␣$␣`` (``␣`` denoting a space character), the rest
  is a command.
- If it starts with ``␣␣>␣``, the rest is the continuation of a command
  (continuation lines must immediately follow a command).
- If it start with ``␣␣`` and something else, the rest is the expected output
  or exit code of the previous command.
- Everything else is a comment.

File and Directory Tests
------------------------

There are two types of Cram tests: file tests and directory tests. File tests
are files with a ``.t`` extension. Directory tests are files named ``run.t``
within a directory with a name that ends with ``.t``.

A Cram test begins its execution in a temporary directory where its
dependencies (as listed in the corresponding :doc:`cram stanzas <dune/cram>`,
if any) are available. In the case of a directory test, the contents of the
directory are also available.

File tests have the nice property that they are self-contained: everything
happens in a single file. This is handy because it does not make a deep file
hierarchy in a project. But if the test requires some files, these need to be
created using ``cat`` and heredocs. Directory tests, on the other hand, allow
creating these test fixtures as normal files. This can be more comfortable
because it makes the usual tooling (syntax highlighting, completion, etc.)
available.

Executing Cram Tests
--------------------

Every Cram test has a name. For file tests, the name of ``something.t`` is
``something``, and for directory tests, the name of ``something.t/run.t`` is
``something``.

There are several ways to execute Cram tests:

- All Cram tests are attached to the :doc:`/reference/aliases/runtest` alias.
  So ``dune runtest`` will run all Cram tests.
- Every Cram test creates an alias after its name. So, ``dune build
  @something`` will run tests named ``something``.

When a Cram test is executed, the commands it contains are executed, and a
corrected file is created where the command outputs are inserted after
each command. This corrected file is then offered for :doc:`promotion
<../concepts/promotion>` by Dune.

Concretely, this means that Dune will display the difference between the
Cram test's current contents and the latest run's output. This diff
can be applied by running ``dune promote``, as usual.

.. code:: diff

   $ touch changed-name.txt
   $ ls
  -other-file.txt
  +changed-name.txt
   this-file.txt
