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
:::

::::

In this case, we want to select a specific revision of the community repository
instead of always using the most recent one as it would do by default. We
define a new repository and configure the lock directory to use this
repository.

For more information about the stanzas refer to the {doc}`repositories stanza
</reference/dune-workspace/repository>` as well as the {doc}`lock_dir stanza
</reference/dune-workspace/lock_dir>`.

When relocking the dependencies, the list of packages that are found as
dependencies changes accordingly:

```
$ dune pkg lock
Solution for .dune-solution-cache:
- base-unix.base
- fmt.0.9.0
- ocaml.5.0.0
- ocaml-base-compiler.5.0.0
- ocaml-config.3
- ocamlbuild.0.15.0+dune
- ocamlfind.1.9.6+dune
- topkg.1.0.
```

Compared to before, the OCaml compiler version is older, which shows
that we did indeed pick an older version of the package repository for locking.

:::{note}
This feature can also be used to make sure the locked dependencies are
reproducible, as fixing all the package repository versions will lead to
equivalent locking results.
:::
