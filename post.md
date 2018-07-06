# Dune 1.0.0 is coming soon, what about jbuilder projects?

The 1.0.0 release of dune is planned for next week. It will be the
first release of jbuilder as dune, so what to expect for existing
jbuilder projects?

The good news is that dune is fully backward compatible with
jbuilder. The `dune` package in opam will install both `dune` and
`jbuilder` binaires and both will be usable on existing jbuilder
projects.

We do want to stop supporting jbuilder projects eventually, so the
plan is to continue to support the `jbuilder` binary and `jbuild`
files for another year. 6 months from now dune will start displaying
warnings when using the `jbuilder` binary or `jbuild` files. A more
detailied [migration plan][mp] is available in the manual. In
particular in contains a precise list of things to do to migrate a
jbuilder project to a dune project.

We hope that the new features in Dune 1.0.0 and subsequent releases
will be enough of an incentive for users to eagerly switch to Dune :)

## Important changes in Dune

This section is not an exhaustive list of changes, but it describes
the important changes to expect when switching from `jbuilder` to
`dune`.

### Versioning and configuration files

A big part of the work in Dune 1.0.0 was getting the versioning story
right. With jbuilder it was hard for us to make the language evolve
without breaking the build of existing projects. Dune can handle that
gracefuly and in the future it will be much easier to introduce
breaking changes.

The versioning of all dune file in a project is controlled via a
single `dune-project` file at the root of the project. This file
contains the version of the dune language and extensions used in the
project.

While this file is required in order to ensure compatibility with
future versions of Dune, it is not required to write one by hand every
time you start a new project or want to do a quick experiment. Dune
will create and edit this file for you as needed, so simply start by
creating a `dune` file (the new `jbuild` files) and dune will take
care of the rest.

Note that the syntax inside `dune`, `dune-project`, ... files is
slightly different from the one insde `jbuild` files. In particular
the language requires many less parentheses and the syntax of
varialbes changed from `${...}` to `%{...}` to avoid issues with shell
commands. This is detailed in the [migration plan][mp] as well.

### Build profiles and default settings

Up to now `jbuilder` supported a `--dev` option in order to enable
stricter flags. There have been several changes regarding this. First
of all you are now able to define as many build profiles as you want:
`dev`, `release`, `perf`, ... These can be selected either via the
`--profile` command line option or via the `dune-workspace` file. You
are also able to define what the default compilation flags mean for
each profile via an [env stanza][env].

Another important change is that the default build profile for the
`dune` binary is `dev` rather than `release`. We found that most of
the time developpers want the development mode by default. Please
remember to add `"-p" name` to all invocations of `dune` in opam
files! This is now even more important than before in order to ensure
the release profile is selected for opam builds.

### Default target

Dune allows to configure what happens when you run `dune build`
without any specific targets. This is done via the introduction of a
[default alias][defal].

### More parellelism by default

If you had `(jobs N)` in you `~/.config/dune/config` file, you can
remove it. Dune now automatically sets the number of jobs that can be
run in parallel to the number of processing units available. The
previous default was 4. For opam files, you should remember to add
`"-j" jobs` in order to let opam decide.

[mp]:    https://dune.readthedocs.io/en/latest/migration.html
[env]:   https://dune.readthedocs.io/en/latest/dune-files.html#env
[defal]: https://dune.readthedocs.io/en/latest/usage.html#default-alias
