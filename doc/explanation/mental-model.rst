#######################
 The Dune Mental Model
#######################

It is not strictly necessary to understand Dune's underlying model to
use it; but knowing how it works under the hood will help writing build
rules, and also help understand some errors and what's possible with
Dune.

.. note::

   This document is a simplification of the reality: the actual rules
   might be different, it does not touch rule loading and glosses over
   how caching works, but should be a useful tool to build an
   understanding of Dune.

****************
 How Dune Works
****************

The building block of Dune is the *rule*:

   A *rule* reads *dependencies* and writes *targets* using an *action*
   (and it can be attached to *aliases*).

When ``dune build`` is executed, it will first read the project's
``dune`` files to determine the rules that apply to the project. Once it
has done this, it will determine what actions it needs to execute to
build the required targets.

************
 An Example
************

Let's take the following example.

-  there's a CLI tool written in OCaml.
-  it has some build-time configuration stored in ``config.json``.
-  it has an integration test, in which the tool is executed with
   ``testdata.txt`` as input.

Configuration Generation
========================

To express the generation of the configuration module we could write:

.. code:: dune

   (rule
    (deps convert/json2ml.exe config.json)
    (target config.ml)
    (action
     (run convert/json2ml.exe config.json -o config.ml)))

This rule will:

-  read its dependencies: ``convert/json2ml.exe`` and ``config.json``
-  and write its target: ``config.ml``
-  using an action: ``(run convert/json2ml.exe config.json -o
   config.ml)``

This rule is very explicit: we write a stanza for a single Dune rule.

Building the Executable
=======================

In contrast, to describe the compilation of the executable, we would
write:

.. code:: dune

   (executable
    (name tool)
    (modules main config))

Here, we use Dune's abstractions. Dune knows about the OCaml compilation
model: the modules need to be compiled and linked together. So it will
generate the following rules under the hood:

-  one rule to compile the ``Main`` module:

   -  it will read its dependency: ``main.ml``
   -  and write its output: ``main.cmx``
   -  using an action: ``(run ocamlopt -c main.ml)``

-  one rule to compile the ``Config`` module:

   -  it will read its dependency: ``config.ml``
   -  and write its output: ``config.cmx``
   -  using an action: ``(run ocamlopt -c config.ml)``

-  one rule to link the ``tool.exe`` executable:

   -  it will read its dependencies: ``main.cmx`` and ``config.cmx``
   -  and write its output: ``tool.exe``
   -  using an action: ``(run ocamlopt -o tool.exe main.cmx
      config.cmx``)

Note that in this example, some files are targets of a rule and
dependencies of another (``.cmx`` files). We are unlikely to ever
interact with them directly, so it can also be useful to think of the
``(executable)`` stanza as a group of rules with ``main.ml`` and
``config.ml`` as inputs and ``tool.exe`` as output.

Running the Tests
=================

Some rules do not produce any output file, but we're still interested in
running their actions. A test is a good example: we want the build
process to exit with an error code if the action fails. In that case,
the rule does not have targets, but we "attach" it to an :term:`alias`,
``runtest`` in this case. This gives us a way of requesting this rule to
be executed. As we are about to see, rules are executed lazily by asking
for their targets to be built, so we would not be able to execute such
rules.

.. code:: dune

   (rule
    (deps tool.exe testdata.txt)
    (alias runtest)
    (action
     (run tool.exe testdata.txt)))

This rule:

-  reads its dependencies: ``tool.exe`` and ``testdata.txt``
-  writes no targets
-  using an action: ``(run tool.exe testdata.txt)``
-  (and it is attached to ``runtest``)

***************
 What to Build
***************

Dune can build *files* and *aliases*. These can be found on the command
line:

-  ``dune build tool.exe`` will build the ``tool.exe`` file.

-  ``dune build @example`` will build the ``example`` alias.

-  ``dune build tool.exe @example`` will build both the file
   ``tool.exe`` and the ``example`` alias.

-  ``dune runtest`` is a shortcut for ``dune build @runtest``: it will
   build the ``runtest`` alias.

-  ``dune build`` is a shortcut for ``dune build @@default``: it will
   build the default alias in the current directory (by default the
   ``all`` alias).

In other words, each ``dune build`` or ``dune runtest`` command always
corresponds to a list of files and aliases to build.

.. seealso::

   :doc:`Reference information on aliases</reference/aliases>`

***************************
 How Dune Interprets Rules
***************************

We have now seen that Dune sets up rules for a project, and that every
build command has a list of files and aliases that we are asking to
build.

Now let's see how this request is processed:

-  to build a file, Dune will first check if it is in the source tree.
   In that case, there is nothing to do. Otherwise, it will check if it
   is the target of a rule. In that case, it will execute this rule.
   (Dune will raise an error in other cases: if the file is both in the
   source tree and the target of a rule, or if it is neither)

-  to build an alias, Dune will execute all the rules that are attached
   to this alias.

-  to execute a rule, Dune will first build all the dependencies (files
   or aliases) of this rule. Then it will execute the action attached to
   the rule. When Dune is about to execute an action, it checks (in
   various caches) if it executed it before on the same set of
   dependencies, and, if yes, it can skip executing it and reuse the
   previous result.

In the case of our example, if we call ``dune runtest``, Dune will
consider all rules attached to the ``runtest`` alias. In this case it is
just the integration test rule. It needs to build its dependencies,
``tool.exe`` and ``testdata.txt``. The latter is present in the source
tree. However, ``tool.exe`` is the target of the linking rule defined by
the ``(executable)`` stanza. This rule requires ``main.cmx`` and
``config.cmx``. ``main.cmx`` is the target of the compilation rule for
the ``Main`` module, which depends on ``main.ml``. This file is in the
source tree, so let's copy it under ``_build``. This rule has all its
dependencies available, so we can run its action, which writes
``main.cmx``. Getting back to the dependencies of ``tool.exe``,
``config.cmx`` is the target of the linking rule of the ``Config``
module. This rule has ``config.ml`` has a dependency. This file is
itself the target of the configuration module rule, which lists
``config.json`` and ``convert/json2ml.exe``. The first is available in
the source tree and to simplify, let's assume that the second one has
been built. This action has all its dependencies available, so we can
execute its action to produce its target, ``config.ml``. Now the module
compilation rule for ``Config`` can be executed, producing
``config.cmx``; and in turn the linking rule can be executed, producing
``tool.exe``. Finally, ``tool.exe`` can be executed with
``testdata.txt`` as its argument.

In a nutshell: we recursively copied all the dependencies of the test
rule, and executed the rules in the correct order.

This is a "cold build", where there were no previous build artifacts.
Note that if we change only part of the project (say the ``main.ml``
file), only a small number of rules will be evaluated, the ones that
depend on ``main.ml``.

************
 Conclusion
************

Dune's underlying model is based on rules. Stanzas are high-level
constructs that can generate multiple rules, that are not always
visible.

To build a target, Dune looks for the rule that produces that target and
makes its way back to source files.

Rules define a directed acyclic graph which models dependency relations
between files. Most of the rules in that graph may be executed for a
cold build, but just the minimum will be executed for an incremental
build.
