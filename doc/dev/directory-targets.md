# Directory targets

A **directory target** corresponds to a file tree rooted at a specified root
directory, for example, `docs/*`. Typical examples of rules producing directory
targets are: unpacking an archive, running `make` in a vendored package, and
building files with non-deterministic names (e.g. including the current date).

## Declaring a directory target

To declare a directory target `docs/*`, use the syntax `(target (dir docs))` in
a rule stanza. The corresponding rule should create the directory `docs` and is
free to populate it with an arbitrary number of files and/or subdirectories.

Like file targets, directory targets can be promoted to the source tree by
adding `(mode promote)` to the rule stanza.

## Depending on a directory target

There are two ways to depend on a directory target:

* An **opaque dependency** on the whole file tree `docs/*`. Opaque dependencies
  are invalidated if the contents of the tree is changed in any way. To declare
  an opaque dependency on `docs/*`, use the syntax `(dep (dir docs))` in a rule
  stanza.

* A **projection dependency** on a specific file in the tree, e.g.
  `docs/html/index.html`. A projection dependency is declared using the standard
  syntax `(dep docs/html/index.html)` and works like a dependency on a normal
  file target. For example, if the `docs/*` directory is rebuilt and only
  `docs/html/logo.png` is modified, then the dependency on `docs/html/index.html`
  is considered to be up-to-date. Note that it is easy to make a mistake with
  such projection dependencies, for example, by forgetting that `index.html`
  actually does include the image `docs/html/logo.png`. In such cases,
  sandboxing will help since only the requested projection dependencies will be
  available in the sandbox (i.e., not the whole directory target).

## Building a directory target

Users can request building whole directory targets or individual files via
`dune build docs` and `dune build docs/html/index.html` commands.

## Current limitations

* It is not allowed to have two rules with the same directory target. That is,
  like file targets, directory targets are **exclusive** (but see _shared
  directory targets_ below).

* Directory targets cannot have nested file or directory targets, i.e. other
  rules are not allowed to declare targets within the file tree of a directory
  target.

## Possible future extensions

Here are some possible extensions to consider:

* **Opaque directory targets**: a rule may declare that its directory target is
  opaque, in which case projection dependencies on its content will be
  disallowed. One can also consider only partially opaque directory targets,
  where the contents of the directory is only partially visible.

* **Shared directory targets**: we can allow multiple rules to write to the same
  directory target, as long as they do not write to the same files. In this
  case, depending on a directory target would mean depending on all of the rules
  that declare it as a target.
