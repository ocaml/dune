# Setting up an OxCaml project

As an example of how to add custom repositories and pin dependencies in projects
using Dune Package Management, let's look at setting up a basic
[OxCaml](https://oxcaml.org/) project. OxCaml is a compiler branch with a fast
moving set of extensions to OCaml. Some tweaks are required to set up a project
that compiles with OxCaml when using Dune package management.

## Setting up the `dune-workspace`

OxCaml has a custom `opam-repository` that provides compatible dependencies.
Dune can use packages from this repositories by configuring it in the
dune-workspace file using the {doc}`repositories stanza
</reference/dune-workspace/repository>` and {doc}`lock_dir stanza
</reference/dune-workspace/lock_dir>`.

```{code-block} dune
(lang dune 3.21)

(pkg enabled)

(repository
 (name oxcaml)
 (url git+https://github.com/oxcaml/opam-repository))

(lock_dir
 (repositories overlay oxcaml upstream))
```

We would like dune to pick the OxCaml compiler rather than the latest upstream
one. This can be achieved by manually adding a constraint on the compiler that
is picked in the {doc}`package stanza </reference/packages>` in
the dune-project.

```{code-block} dune
(lang dune 3.21)

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

Let's compiler and execute the project. Note that when you run `dune build` the
very first time, Dune pulls in and builds all the dependencies before building
the project itself. It is expected to take a considerable amount of time.

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

Please see {doc}`/howto/customize-dev-tools-lock-directories` for details about
configuring your workspace for developer tools.
