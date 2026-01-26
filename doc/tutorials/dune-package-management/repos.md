# Custom Repositories

By default when locking package versions, Dune looks up packages from two
sources:

  1. The upstream, community maintained `opam-repository` at
     [ocaml/opam-repository](https://github.com/ocaml/opam-repository) for most
     packages
  2. An overlay repository with patched software to allow usage in Dune at
     [ocaml-dune/opam-overlays](https://github.com/ocaml-dune/opam-overlays)

To change the presets, the `dune-workspace` file has to be edited (and created
if it didn't exist):

::::{dropdown} `dune-workspace`
:icon: file-code

:::{literalinclude} repos/dune-workspace
:language: dune
:emphasize-lines: 5-6,8-10
:::

::::

In this case, we want to select a specific revision of the community repository
instead of always using the most recent one as it would do by default. We
define a new repository and configure the Dune solver to use this
repository.

For more information about the stanzas refer to the {doc}`repositories stanza
</reference/dune-workspace/repository>` as well as the {doc}`lock_dir stanza
</reference/dune-workspace/lock_dir>`.

The next time the build system is run, instead of using the default
repositories at their newest versions, the solver will check out the configured
repositories at the defined revisions. These will then be used to determine the
new solution, which will get used for downloading and building the
dependencies.

```sh
dune build
```

will thus log a new solution in `_build/log`. Note that a lot of package
versions are different, as the state of the opam-repository is frozen at the
specific commit:

```
...
# Dependency solution for
# _build/.sandbox/<sandbox-hash>/_private/default/.lock/dune.lock:
# - base-unix.base
# - fmt.0.9.0
# - ocaml.5.0.0
# - ocaml-base-compiler.5.0.0
# - ocaml-config.3
# - ocamlbuild.0.16.1+dune
# - ocamlfind.1.9.8+dune
# - topkg.1.0.7
...
```

:::{note}
This feature can also be used to make sure the locked dependencies are
reproducible, as fixing all the package repository versions will lead to
equivalent locking results.
:::

:::{seealso}
{doc}`/tutorials/dune-package-management/locking`
  More information how to keep package versions locked.
:::
