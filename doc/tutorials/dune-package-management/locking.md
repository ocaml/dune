# Locking Your Dependencies

In the default use-case Dune will automatically determine which packages to
install, by reading the package constrains, determining compatible versions and
installing the dependencies automatically.

For many projects this is a good and acceptable behavior as users often want to
use new versions of their dependencies. However some projects might want to
keep a fixed set of (transitive) dependencies that is only updated manually.

## Create a lock directory manually

If a lock directory exists in the source, Dune will use that to fix the exact
version and source of dependencies. The default name of said lock directory is
`dune.lock`. Lock directories are created with:

```
$ dune pkg lock
Solution for dune.lock:
- ocaml.5.2.0
- ocaml-base-compiler.5.2.0
- ocaml-config.3
```

Whenever Dune encounters a `dune.lock` folder, it will use the set of
dependencies defined in the lock. It contains all the metadata about package
names and versions, their dependencies and source locations that are necessary
to build the project's dependencies.

On the next build, Dune will read the stored solver solution from the
`dune.lock` directory, download and build the dependencies and then continue on
building the project as usual.

The lock directory will not be updated until `dune pkg lock` is rerun.

:::{note}
This approach is similar to using `opam switch export --full --freeze` to
export the configuration of a switch.
:::

Deleting the lock directory will cause Dune to fall back to automatically
determining dependency versions via the declared package constraints.
