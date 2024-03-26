Using Rule Generation
=====================

Sometimes it can be useful to generate Dune rules that depend on the file system
layout or on the content of configuration files. This often happens for
integration tests.

In this document, we will see two ways to encode this behavior.

We suppose that we are testing an executable named ``tool``. There are some
input files named ``*.input``, output files named ``*.output``, and we want to
ensure that when running ``tool`` on ``x.input``, the standard output
corresponds to ``x.output``.

The Generate-Include-Commit Pattern
-----------------------------------

.. note::

   This is the most common way to do this. It has a couple drawbacks listed
   below, but you should start with this pattern.

What we are going to do is:

- generate a ``dune.inc`` file;
- include it in our main ``dune`` file;
- commit the generated code in the source repository.

This creates a loop: a program (the "generator") looks at the file system and
creates a ``dune.inc`` file. Changes to the file system (for example, if a test
is added) mean that a change in ``dune.inc`` will be promoted. These generated
rules are included in the main ``dune`` file, so ``dune runtest`` will run the
tests. Finally, the generated file is part of the source repository, so it is
not necessary to run several commands to run the test suite.

Let's expand a bit on how to achieve this.

Generating a ``dune.inc`` File
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Create a ``gen`` subdirectory and create a ``gen.ml`` file in it:

``gen/gen.ml``
  .. code:: ocaml

    let generate_rules base =
      Printf.printf
        {|
            (rule
             (with-stdout-to %s.gen
              (run %%{bin:tool} %s.input)))
        
            (rule
             (alias runtest)
             (action
              (diff %s.output %s.gen)))
             |}
        base base base base

    let () =
      Sys.readdir "."
      |> Array.to_list
      |> List.sort String.compare
      |> List.filter_map (Filename.chop_suffix_opt ~suffix:".input")
      |> List.iter generate_rules

Create a ``dune`` file in that directory:

``gen/dune``
  .. code:: dune

    (executable
     (name gen))

This defines an executable that lists ``*.input`` files in the current
directory and outputs rules on its standard output.

.. note::

   It is important to sort the input files to ensure that the output is
   independent from the order in which ``Sys.readdir`` returns the files .

For each input file, we output two rules:

- The first one creates a ``x.gen`` file that corresponds to the actual output.
- The second uses a :doc:`/reference/actions/diff` action to compare the actual output to the expected output. If it is different, ``dune runtest`` will display the difference, which can be accepted by ``dune promote``.

.. note::

   It is possible to have more complicated logic here. For example, to pass
   different arguments to ``tool`` depending on the presence of a ``*.args``
   file. To do that, check if ``*.args`` exists in ``generate_rules`` and emit
   a different ``(run ...)`` action.

Including it in the Main ``dune`` File
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Our main test ``dune`` file contains the following:

``dune``
  .. code:: dune

    (include dune.inc)

    (rule
     (deps (source_tree .))
     (with-stdout-to
      dune.inc.gen
      (run gen/gen.exe)))

    (rule
     (alias runtest)
     (action
      (diff dune.inc dune.inc.gen)))

In addition to including the contents of ``dune.inc``, we use the same pattern
as before: ``dune.inc.gen`` is the actual output of the generator, and
``dune.inc`` is the expected output. At runtime, the generator will read the
contents of the current directory (where the ``*.input`` and ``*.output`` files
are located), so we record ``(source_tree .)`` as a dependency to make it run
again if a file is created, for example.

Commit the Generated Code In The Source Repository
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To make this work, we have a final step to do. We have to add the generated
file to our source tree. But since it is generated, we will have to first
create an empty file, run the test, and promote the result.

.. code:: console

   $ touch dune.inc
   $ dune runtest
   + (rule
   +  (with-stdout-to a.gen
   +   (run %{bin:tool} a.input)))
   +
   + (rule
   +  (alias runtest)
   +  (action
   +   (diff a.output a.gen)))
   $ dune promote dune.inc
   $ git add dune.inc

Now, running ``dune runtest`` will run the test suite.

Notes
^^^^^

This pattern is "correct": it will execute all tests and make sure the
list of tests is up to date. But when adding a test, it is necessary to first
run ``dune runtest``, promote the result, and then re-run ``dune runtest`` to
actually run the test (and possibly promote the result of the test itself).

There is a variant of this pattern which will promote the output automatically
instead of using a manual promotion step. This variant can be used either for
the test list or for the individual tests.

To use it in the test list, replace the ``dune`` file by this version:

``dune`` (alternative version)
  .. code:: dune

    (include dune.inc)

    (rule
     (mode promote)
     (alias runtest)
     (deps (source_tree .))
     (with-stdout-to
      dune.inc
      (run gen/gen.exe)))

Using this version, ``dune runtest`` will directly replace ``dune.inc`` with an
updated version.

Another caveat of this approach is that the generator needs to emit the same
output on all systems. For example, if some tests should be skipped on Linux,
the generator can not just filter the corresponding tests depending on
``Sys.os_type``. It has to consistently emit a ``(enabled_if)`` field for the
rules.

Using ``(dynamic_include)``
---------------------------

.. versionadded:: 3.14

This technique relies on :doc:`/reference/dune/dynamic_include`, which is
more flexible than :doc:`/reference/dune/include`. The difference is that
the intermediate ``dune.inc`` file does not need to be part of the source tree.
It will only be generated by a rule and be present in the ``_build`` directory.

At first it looks like it would be possible to reuse the same pattern as above:
change ``include`` to ``dynamic_include`` and delete the ``dune.inc`` file.
However, it is not possible. The reason is that rules are loaded per directory,
and there needs to be a strict order (no cycles) between directories for this
to work.

So, instead we are going to:

- generate ``dune.inc`` in a subdirectory named ``generate``, and
- include these rules in a subdirectory named ``run``.

These subdirectories do not need to be actual directories. They can be emulated
through :doc:`/reference/dune/subdir`.

To do this, we can create the following ``dune`` file in the same directory as
the ``*.input`` and ``*.output`` files.

``dune``
  .. code:: dune

    (executable
     (name gen))
    
    (subdir run
     (dynamic_include ../generate/dune.inc))
    
    (subdir generate
     (rule
      (deps (glob_files ../*.input))
      (action
       (with-stdout-to dune.inc
        (run ../gen.exe)))))

Then create the following ``gen.ml`` file. Note that here we can define it in
the same directory.

``gen.ml``
  .. code:: ocaml

    let generate_rules base =
      Printf.printf
        {|
            (rule
             (with-stdout-to %s.gen
              (run %%{bin:tool} ../%s.input)))
        
            (rule
             (alias runtest)
             (action
              (diff ../%s.output %s.gen)))
             |}
        base base base base
    
    let () =
      Sys.readdir ".." |> Array.to_list |> List.sort String.compare
      |> List.filter_map (Filename.chop_suffix_opt ~suffix:".input")
      |> List.iter generate_rules

There are a few differences from the generator above because this one
is going to be invoked from subdirectories, so it is necessary to refer to the
``..`` directory both in the input (which files to read) and in the output (how
the rules are executed).

These two files are enough. ``dune runtest`` is going to generate the rules and
interpret them in a single command.

Notes
^^^^^

This approach is shorter, but it might be more difficult to debug because changes
to the generated rules will not be visible. Also, it works in that case, but it
is not possible to generate all kinds of stanzas with that pattern. See
:doc:`/reference/dune/dynamic_include` for more information about the
limitations.
