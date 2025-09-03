# How Package Management Works

This document explains how Dune's package management works under the hood. It
requires a bit of familiarity with how opam repositories work and how Dune
builds packages. Thus it is aimed at people who want to understand how the
feature works, not how it is used.

For a tour on how to apply package management to a project, refer to the
{doc}`/tutorials/dune-package-management/index` tutorial.

## Motivation

A core part of modern programming is using existing code to save time. The
OCaml package ecosystem has quite a long history with many projects building
upon each other over many years. A significant step forward was the creation of
the OCaml Package Manager, opam, along with the establishment of a public
package repository which made it a lot more feasible to share code between
people and projects.

Over time, best practices have evolved, and while opam has incorporated some
changes, it couldn't adopt all the modern workflows due to its existing user
base and constraints.

Thus the Dune Package Management has been designed with a few core goals in
mind:

* No global state visible to users, everything is local to projects
* Package management is configured through files (`dune-project` and optionally
  `dune-workspace`)
* Repositories are automatically kept up to date unless explicitly configured
  to use specific versions
* Builds can only access packages they have declared dependencies on
* Reproducible builds through lockfiles

Dune plays well with the existing OCaml ecosystem and does not introduce a new
type of packages. Rather, it uses the same package repository and Dune packages
stay installable with opam.

## Package Management in a Project

This section describes what happens in a Dune project using the package
management feature.

### Dependency Selection

The first step is to determine which packages need to be installed.
Traditionally this has been defined in the `depends` field of a project's opam
file(s).

Since version 1.10 Dune has supported {doc}`opam file generation
</howto/opam-file-generation>` by specifying the package dependencies in the
`dune-project`.

The package management feature uses the same metadata, as Dune will determine
the list of packages to install from the `depends` field in the `dune-project`
file. This allows projects to completely omit generation of `.opam` files, as
long as they use Dune for package management. Thus all dependencies on OCaml
packages are only declared in one single file.

To maintain compatibility with a large number of existing projects, Dune
continues to support `.opam` files. While it is recommended to declare the
dependencies directly in the `dune-project` file, it is not mandatory to do so.
Dune will fall back to reading dependencies from `.opam` files when the package
is not defined in `dune-project`.


### Locking

Given the list of the project's dependencies and their version
constraints, the next steps are:

1. Find the transitive dependencies and figure out a version for each
   dependency that satisfies the constraints
2. For each dependency, download it, build it, and make it available to the
   project

In opam, `opam install` does both of these.

In Dune, these are separate steps: the first one is `dune pkg lock`, and the
second one happens implicitly as part of [building](#building).

The idea of doing the first step and recording it for later is popular in other
programming language package managers like NPM and is usually called locking.
Creating a lock file ensures that the dependencies to be installed are
always the same - unless that lock file is updated of course.

:::{note}
`opam` also supports creating lock files. However, these are not as central to
the opam workflow as they are in the case of package management in Dune, which
always requires a set of locked packages.
:::

In the most general sense, a lock file is just a set of specific packages
and their versions to be installed.

Instead of a lock file, Dune writes this information to a directory (the "lock
directory") with files that describe the dependencies. It includes the
package's name and version. Unlike many other package managers, the files
include a lot of other information as well, such as the location of the source
archives to download (since there is no central location for all archives), the
build instructions (since each package can use its own way of building), and
additional metadata like the system packages it depends upon.

The information is stored in a directory (`dune.lock` by default) as separate
files, to reduce potential merge conflicts and simplify code review. Storing
additional files like patches is also simpler this way.

#### Package Repository Management

To find a valid solution that allows a project to be built, it is necessary to
know what packages exist, what versions of these packages exist, and what other
packages these depend on, etc.

In opam, this information is tracked in a central repository called
[`opam-repository`](https://github.com/ocaml/opam-repository), which contains
all the metadata for published packages.

It is managed using Git; opam typically uses a snapshot to find the
dependencies when searching for a solution that satisfies the constraints.

Likewise, Dune uses the same repository; however, instead of snapshots of the
contents, it uses the Git repository directly.

:::{note}
Dune maintains a shared internal cache containing all Git repositories that
projects use. This way updates and checkouts are very fast because only new
revisions have to be retrieved. The downside is that to be included in the
cache, all the Git repos have to be cloned first which depending on the size of
the repositories can take a bit of time.
:::

On every call to `dune pkg lock`, Dune will update the metadata repository
first (hence why efficiently updating that repository matters). This means that
each `dune pkg lock` will use the newest set of packages available.

However, it is also possible to declare specific revisions of the repositories,
to get a reproducible solution. Due to using Git, any previous revision of the
repository can be used by specifying a commit hash.

Dune uses two repositories by default:

* `upstream` refers to the default branch of `opam-repository`, which contains
  all the publicly released packages.
* `overlay` refers to
  [opam-overlay](https://github.com/ocaml-dune/opam-overlays), which defines
  packages patched to work with package management. The long-term goal is to
  have as few packages as possible in this repository as more and more packages
  work within Dune Package Management upstream. Check the
  [compatibility](#compatibility) section for details.

#### Solving

After Dune has read the constraints and loaded set of candidate packages, it is
necessary to determine which packages and versions should be selected for the
package lock.

To do so, Dune uses
[`opam-0install-solver`](https://github.com/ocaml-opam/opam-0install-solver),
which is a variant of the [`0install`](https://github.com/0install/0install)
solver to find solutions for opam packages.

Contrary to opam, the Dune solver always starts from a blank slate; it assumes
nothing is installed and everything needs to be installed. This has the
advantage that solving is now simpler, and previous solver solutions don't
interfere with the current one. Thus, given the same inputs, it should always
come up with the same result; no state is held between the solver runs.

This can lead to more packages being installed (as opam won't install new
package versions by default if the existing versions satisfy the constraints),
but it avoids interference from already installed packages that lead to
potentially different solutions.

After solving is done, the solution gets written into the lock directory with
all the metadata necessary to build and install the packages. From this point
on, there is no need to access the package metadata repositories.

:::{note}
Solving and locking does not download the package sources. These are downloaded
in the build step.
:::

(building)=
### Building

When building, Dune will read the information from the lock directory and set
up rules for the packages. Check {doc}`/explanation/mental-model` for details
about rules.

The rules that the package management sets up include:

* Fetch rules to download and unpack the source archives, and also download any
additional sources such as patches
* Build rules to execute the build instructions stored in the lock directory
* Install rules to put the artifacts that were built into the appropriate
  Dune-managed folders

Creating these processes as rules mean that they will only be executed on
demand, so if the project has already downloaded the sources, it does not need
to download them again. Likewise, if packages are installed, they stay
installed.

The results of the rules are stored in the project's `_build` directory and
managed automatically by Dune. Thus, when cleaning the build directory, the
installed packages are cleaned as well and will be reinstalled at the next
build.

(compatibility)=
## Packaging for Dune Compatibility

Dune can build and install most packages as dependencies, even if they are not
built with Dune themselves. Dune will execute the build instructions from the
lock directory, very similar to opam.

However, packages must adhere to certain rules to be compatible with Dune.

The most important one is that the packages must not use absolute paths to
refer to files. That means they cannot read the path they are being built or
installed in and expect this path to remain the same. Dune builds packages in a
sandbox location, and after the build has finished, it moves the files to the
actual destination.

:::{note}
Unlike opam, Dune at the moment does not wrap the build in sandboxing tools
like [Bubblewrap](https://github.com/containers/bubblewrap).
:::

To comply with these restrictions the usual solution is to use relative paths,
as Dune guarantees that packages installed into different sections are
installed in a way where their relative location stays the same.

The `overlay` repository exists specifically to make currently non-compliant
packages compatible with Dune's package management. It does so by supplying
releases of packages where the current upstream releases don't support Dune
package management yet.
