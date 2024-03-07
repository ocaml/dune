How Preprocessing Works
=======================

Preprocessing consists in transforming source code before it is compiled. The
goal of this document is to explain how this works in Dune.

Dune supports two separate ways of applying preprocessors, the "classic pipeline" (used
with ``(staged_pps)``), and the "fast pipeline" (used for all other
:doc:`preprocessing specifications <../reference/preprocessing-spec>` including
``(pps)``).

The OCaml compilers provide options for specifying a preprocessing step. The
``-pp`` option is used to invoke a textual preprocessor (something that reads
text and returns text). The ``-ppx`` option is used to invoke a `ppx rewriter`
(a function that takes an AST and outputs an AST).

This is the "classic pipeline": preprocessing is part of the compilation
itself. This is simple, but has a problem: in order to compute the dependencies
of a module, it is necessary to pass the same ``-pp`` or ``-ppx`` option to
``ocamldep``.

The classic pipeline has the following steps:

- preprocessing (as part of ``ocamldep``)
- dependency analysis
- preprocessing (as part of compilation)
- compilation

Dune supports a "fast pipeline" where the preprocessor is invoked separately
from the compiler and its output is saved. Afterwards the preprocessed code is
compiled directly.

The fast pipeline has the following steps:

- preprocessing
- dependency analysis
- compilation

It has several advantages: it only invokes the preprocessor once per file, and
the preprocessed code is reused between dependency analysis and different kinds
of compilation. Also, when several preprocessors use ``ppxlib``, they can be
combined in a preprocessing program that traverses the AST only once.

However, some specific code generators or preprocessors require direct
access to the compilation artefacts of their dependencies. Therefore they
need to be used with the classic pipeline, even if it is slower. Note that a
PPX is able to know if it was called as part of ``ocamldep -ppx`` or ``ocamlopt
-ppx``, so it can act differently in each phase.

Dune chooses which pipeline to use depending on the
provided :doc:`../reference/preprocessing-spec`. It will select the fast pipeline,
unless ``(staged_pps)`` is used. In that case, the classic pipeline is used.

In the case of the fast pipeline, a single executable is built and accepts
arguments for all preprocessors.
