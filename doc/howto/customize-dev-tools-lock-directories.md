# How to Customize Lock Directories of Developer Tools

The package management support in Dune has a feature for installing developer
tools. These tools are useful for working on the code of the project but not
necessary for the deployment of the code. Examples of such tools include
[utop](https://github.com/ocaml-community/utop), the [OCaml LSP
server](https://github.com/ocaml/ocaml-lsp) or
[ocamlformat](https://github.com/ocaml-ppx/ocamlformat).

In general these tools do not require any specific configuration: Dune will
create a lock dir implicitely and install a compatible version of the tool
depending on its availability in
[opam-repository](https://github.com/ocaml/opam-repository).

:::{note}
The configuration of lock directories of developer tools is in an
*experimental* state. It is possible that before stabilizing the configuration
format might be changed. This document solely documents the current state of
the lock configuration with no guarantees about future compatibility.

In case you encounter issues please [open an
issue](https://github.com/ocaml/dune/issues) in the Dune issue tracker.
:::

However, in some cases it might be necessary to override the configuration of
the developer tool. This might for example be if a special fork of the
tool is to be used, if a specific version should be picked or if additional
constraints are required.

Configuring the lock directory for a developer tool works in the same way as
configuring any other lock directory, via the {doc}`lock_dir stanza
</reference/dune-workspace/lock_dir>`. The difference however is, that the lock
dir path for a lock dir cannot be chosen freely and must match the interal path
that Dune will pick for the lock directory of said developer tool.

The format of the lock dir path is
`<build-dir>/.dev-tools.locks/<name-of-opam-package>`. By default the
`<build-dir>` will be `_build`, unless it is configured otherwise.

## A trivial example

Thus the potentially simplest configuration for a customized config for
ocamlformat is this definition in `dune-workspace`:

```{code-block} dune
(lang dune 3.21)
(pkg enabled)

(lock_dir
 (path "_build/.dev-tools.locks/ocamlformat"))
```

The `lock_dir` stanza defines that the configuration to be set is
`_build/.dev-tools.locks/ocamlformat`, thus the lock directory location that
Dune will pick when attempting to create a lock directory and run the solver
for ocamlformat.

However, given the lock dir does not contain any additional definitions this
does not customize anything.

## Adding more customization

Thus a more complete example which exercises a lot of useful options to
configure a custom ocamlformat can look like this `dune-workspace`
configuration:

```{code-block} dune
(lang dune 3.21)
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

(lock_dir
 (path "_build/.dev-tools.locks/ocamlformat")
 (pins ocamlbuild)
 (constraints
  (ocaml-lsp-server (= 1.19.0+ox))
  (ocaml-variants (= 5.2.0+ox)))
 (repositories overlay oxcaml upstream))
```

It specifies that that lock tool should use a pinned version of `ocamlbuild` as
well as custom repositories in a specific order (adding the `oxcaml` repository
in between the repositories that are defined out of the box). Constraints are
added on the versions of other packages to be selected for for building
ocamlformat.

:::{seealso}
{doc}`/reference/dune-workspace/lock_dir`.
  Reference for the `lock_dir` stanza, documenting all possible customization
  options.
:::
