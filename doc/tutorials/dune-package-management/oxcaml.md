# Setting up an OxCaml project

As an example of how to add custom repositories and pin dependencies in projects
using `dune pkg`, let's look at setting up a basic OxCaml project.
[OxCaml](https://oxcaml.org/) is a compiler branch with fast moving set of
extensions to OCaml. Some tweaks are required to set up a project that compiles
with OxCaml when using dune package management.

## Setting up the `dune-workspace`

OxCaml has a custom `opam-repository` that provides compatible dependencies.
This is specified in the `dune-workspace`.

```{code-block} dune
(lang dune 3.20)

(pkg enabled)

(repository
 (name oxcaml)
 (url git+https://github.com/oxcaml/opam-repository))

(lock_dir
 (repositories overlay oxcaml upstream))
```

In the `dune-project`, we also need to add a constraint to depend on the OxCaml
compiler. Otherwise, the solver may pick up the latest upstream compiler, which
does not support the OxCaml extensions.

```{code-block} dune
(lang dune 3.20)

(package
 (name hello-oxcaml)
 (depends
  (ocaml-variants
   (= 5.2.0+ox))))
```

Let's add a test program that uses OxCaml syntax.

::::{dropdown} `main.ml`
:icon: file-code

:::{literalinclude} oxcaml/main.ml
:language: dune
:::

::::

And to build it, we add a `dune` file.

```{code-block} dune
(executable
 (public_name main))
```

## Building the project

As usual, let's build the project with:

```
$ dune exec ./main.exe
...
43
```
We have successfully built the project with the OxCaml compiler and run the executable.

## Developer tools

Note that OxCaml provides its own forks of the developer tools
`ocaml-lsp-server`, `ocamlformat`, etc. Make sure to include constraints for the
tools in the `dune-workspace` if you intend to use them for your development
workflows.

For `ocamlformat` it would look something like this:

```{code-block} dune

(lang dune 3.20)

(pkg enabled)

(repository
 (name oxcaml)
 (url "git+https://github.com/oxcaml/opam-repository"))

(pin
 (name ocamlbuild)
 (url "git+https://github.com/Sudha247/ocamlbuild#oxcaml+dune")
 (package
   (name ocamlbuild)
   (version 0.15.0+ox)))

(pin
 (name odoc-parser)
 (url "git+https://github.com/Sudha247/odoc#replace-symlinks")
 (package
   (name odoc-parser)
   (version 3.1.0+ox)))

(lock_dir
 (path "dune.lock")
 (repositories :standard oxcaml))

(lock_dir
 (path "dev-tools.locks/ocamlformat")
 (pins ocamlbuild)
 (constraints
   (ocaml-lsp-server (= 1.19.0+ox))
   (ocaml-variants (= 5.2.0+ox)))
 (repositories overlay oxcaml upstream))
```

Observe that this includes pins to dependencies `ocamlbuild` and `odoc-parser` as they are required by the OxCaml version of `ocamlformat`.