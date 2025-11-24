# Example: Setting up an OxCaml project

As an example to adding custom repositories and pinning projects, let's look at
setting up a basic OxCaml project. [OxCaml](https://oxcaml.org/) is a compiler
branch with fast moving set of extenstions to OCaml. Some tweaks are required to
set up an OxCaml project with dune package management.

## Setting up the workspace

OxCaml has a custom `opam-repository` that we need in order to pull in its
dependencies. This is specified in the `dune-workspace`.

::::{dropdown} `dune-workspace`
:icon: file-code

:::{literalinclude} oxcaml/dune-workspace
:language: dune
:emphasize-lines: 3-5
:::

::::

In the `dune-project`, we also need to add a constraint to depend on the OxCaml
compiler. Otherwise, the solver may pick up the latest upstream compiler, which
does not support the OxCaml extensions.

::::{dropdown} `dune-project`
:icon: file-code

:::{literalinclude} oxcaml/dune-project
:language: dune
:::

::::

Let's add a test program that uses OxCaml syntax.

::::{dropdown} `main.ml`
:icon: file-code

:::{literalinclude} oxcaml/main.ml
:language: dune
:::

::::

And to build it, we add a `dune` file.

::::{dropdown} `dune`
:icon: file-code

:::{literalinclude} oxcaml/dune
:language: dune
:::

::::

## Building the project

As usual, let's lock the dependencies and build the project with:

```
$ dune pkg lock
Solution for dune.lock:                                                        
- conf-autoconf.0.2
- conf-which.1
- ocaml-variants.5.2.0+ox

$ dune build && dune exec ./main.exe
...
43
```
We have successfully built the project with the OxCaml compiler.

## Developer tools

Note that OxCaml provides its own forks of the developer tools
`ocaml-lsp-server`, `ocamlformat`, etc. Make sure to include constraints for the
tools in the `dune-workspace` if you intend to use them for your development
workflows.
