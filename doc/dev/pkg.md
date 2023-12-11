# Package Management in Dune

- [Package Management in Dune](#package-management-in-dune)
  - [Introduction](#introduction)
  - [Reference](#reference)
    - [Stanza Reference](#stanza-reference)
      - [`lock_dir`](#lock_dir)
    - [CLI Reference](#cli-reference)
      - [`dune pkg lock`](#dune-pkg-lock)
      - [`dune pkg outdated`](#dune-pkg-outdated)
      - [`dune pkg print-solver-env`](#dune-pkg-print-solver-env)
      - [`dune pkg validate-lockdir`](#dune-pkg-validate-lockdir)
      - [`dune describe pkg lock`](#dune-describe-pkg-lock)
      - [`dune describe pkg list-locked-dependencies`](#dune-describe-pkg-list-locked-dependencies)
      - [`dune describe pkg dependency-hash`](#dune-describe-pkg-dependency-hash)
  - [User Workflows](#user-workflows)
    - [Creating and building a project](#creating-and-building-a-project)


This document describes the package management support that is currently being
develop for Dune. It will serve as a guide for developers who will write the
user-facing documentation, therefore we can be as technical as we like.

## Introduction

Package management in Dune refers to the features that allow users to lock
package dependencies from an opam repository and build them locally in a
reproducible way.

## Reference

### Stanza Reference

#### `lock_dir`

This stanza goes in `dune-workspace` and specifies a lock directory. The syntax
is:

```
(lock_dir
 (path <path>)
 (solver_env <solver_env>)
 (version_preference <version_preference>)
 (constraints <constraints>))
```

where:
- `<path>` is the path to the lock directory. It is relative to the workspace
  root.

- `<solver_env>` is the solver enviornement that will be used. It contains a
  list of opam variables and their values.

- `version_preference` can be `newest` or `oldest`. It specifies whether the
  newest or oldest version of a package should be used when solving.

- `<constraints>` is a list of constraints that will be used to solve the
  package dependencies. It is a list of `(package <package> <constraint>)`
  where `<package>` is the name of the package and `<constraint>` is a
  constraint on the version of the package.

### CLI Reference

#### `dune pkg lock`

This file creates a lock directory. It  can take lock directories as positional
arguments and it will create them. The default lock directory is `dune.lock/`.

It will first download the opam repository, which on the first download will
take longer, but will be cached for subsequent runs. It will then solve the
dependencies and write the lock directory.

#### `dune pkg outdated`

This command displays the packages in a lock directory that are outdated. It can
take lock directories as positional arguments. The default lock directory is
`dune.lock/`. It can take a `--transitive` flag to display the outdated
transitive dependencies as well.

#### `dune pkg print-solver-env`

This command prints a description of the enviornment that would be used to solve
dependencies. This is useful for debugging a situation where the solver is
unable to find a solution.

#### `dune pkg validate-lockdir`

This command validates a lock directory. It makes sure that the lock directory
is sound otherwise it suggests that `dune pkg lock` should be run again.

#### `dune describe pkg lock`

This command prints the contents of the lock directory. It can take lock
directories as positional arguments. The default lock directory is `dune.lock/`.

#### `dune describe pkg list-locked-dependencies`

This command lists the dependencies that are locked in the lock directory. It
can take a `--transitive` flag to list the transitive dependencies as well. It
can take lock directories as positional arguments. The default lock directory is
`dune.lock/`.

#### `dune describe pkg dependency-hash`

TODO

## User Workflows

### Creating and building a project

When a user has an OCaml project with a `dune-project` file that contains a
`(package)` stanza, they can run:

```console
dune pkg lock
```

In order to create the `dune.lock/` directory. This directory describes how
package dependencies can be built by Dune and can be committed to version
control.

When a user wants to build the project, they can run:

```console
dune build @install
```

and this will build the project and its dependencies.

