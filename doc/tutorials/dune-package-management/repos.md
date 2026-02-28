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

:::{note}
This feature can also be used to make sure the locked dependencies are
reproducible, as fixing all the package repository versions will lead to
equivalent locking results.
:::

:::{seealso}
{doc}`/tutorials/dune-package-management/locking`
  More information how to keep package versions locked.
:::
