# Locking Your Dependencies

In the default use-case Dune will automatically determine which packages to
install, by reading the package constraints, determining compatible versions and
installing the dependencies automatically.

For many projects this is a good and acceptable behavior as users often want to
use new versions of their dependencies. However some projects might want to
keep a fixed set of (transitive) dependencies that is only updated manually.

## Creating a lock directory

If a lock directory exists in the source, Dune will use that to fix the exact
version and source of dependencies. The default name of said lock directory is
`dune.lock`.

Lock directories are created with:

```
$ dune pkg lock
Solution for dune.lock:
- ocaml.5.2.0
- ocaml-base-compiler.5.2.0
- ocaml-config.3
```

Whenever Dune encounters a `dune.lock` folder, it will use the set of
dependencies defined in the lock. It contains all the metadata necessary
to build a project's dependencies, including every packages' name, version, 
dependencies, source location.

On the next build, Dune will read the stored solver solution from the
`dune.lock` directory, download and build the dependencies, and then continue to
build the project as usual.

:::{note}
This approach is similar to using `opam switch export --full --freeze` to
export the configuration of a switch.
:::

## Updating a lock directory

To update a lock directory, rerun

```
$ dune pkg lock
```

All the dependencies in the lock directory will be updated to the latest 
available versions that remain consistent with the declared constraints.

## Removing a lock directory

Deleting the lock directory will cause Dune to fall back to automatically
determining dependency versions via the declared package constraints.
