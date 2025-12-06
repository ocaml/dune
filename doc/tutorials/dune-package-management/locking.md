# Locking Your Dependencies

In the default use-case Dune will automatically determine which packages, their
versions and their recursive dependencies to install. However, given that new
packages are published in the opam-repository occasionally, Dune might pick
different solutions over time.

For many projects this is a good and acceptable behavior as users often want to
use new versions of their dependencies. However some projects might want to
keep a consistent list of dependencies that is only updated manually. This can
be done in multiple ways.

## Create a lock file explicitely

This approach will run the solver with your current package dependency
definitions and create a directory named `dune.lock` which contains the result
of running the solver.

```
$ dune pkg lock
Solution for dune.lock:
- ocaml.5.2.0
- ocaml-base-compiler.5.2.0
- ocaml-config.3
```

Whenever Dune encounters a `dune.lock` folder, it will use the solution defined
in that folder instead of running the solver again. This way the solution will
only get updated when the user runs the command.

On the next Dune build, Dune will read the solution from the `dune.lock`
directory, download and build the dependencies and then continue on building
the project as usual.

:::{note}
This approach is similar to using `opam switch export --full --freeze` to
export the configuration of a switch.
:::

## Lock the inputs

Another way to ensure the solution that Dune will pick is to make sure the
inputs to the solver are identical.

::::

::::{dropdown} `dune-workspace`
:icon: file-code

:::{literalinclude} locking/dune-workspace
:language: dune
:::

Dune is instructed to use very specific versions of the repositories to create
a new solution.

::::

On the next build with Dune, Dune will create a new solution by checking out
these specific two repositories and running the solver. It will then download
and build the dependencies as usual.

:::{note}
This approach is similar to pinning OPAM repositories by explicitely setting
them to know versions using commands like `opam repository add <name> <fixed-url>`.
:::
