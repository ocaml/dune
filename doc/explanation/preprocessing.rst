How Preprocessing Works
=======================

Preprocessing consists in transforming source code before it is compiled. The
goal of this document is to explain how this works in Dune.

The OCaml compilers provide two options for specifying a preprocessing step.
The ``-pp`` option is used to invoke a textual preprocessor (something that
reads text and returns text). The ``-ppx`` option is used to invoke a `ppx
rewriter` (a function that takes an AST and outputs an AST).

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
from the compiler and its output is saved. Then, the preprocessed code is
compiled directly.

The fast pipeline has the following steps:

- preprocessing
- dependency analysis
- compilation

It has several advantages: it only invokes the preprocessor once per file, and
the preprocessed code is reused between dependency analysis and different kinds
of compilation. Also, when several preprocessors use ``ppxlib``, they can be
combined in preprocessing program that traverses the AST only once.

However, some specific code generators or preprocessors require feedback from
the compilation phase. This is the case for PPX rewriters using the OCaml
typer, for instance. As a result, they need to be used with the classic
pipeline, even if it is slower.

Dune chooses the pipeline to use depending depending on the
:doc:`../reference/preprocessing-spec` used. It will use the fast pipeline,
except if ``(staged_pps)`` is used. In that case, the classic pipeline is used.

In the case of the fast pipeline, a single executable is built and accepts
arguments for all preprocessors.
