# Dune Tools Requirements

Authors: Ali Caglayan (Tarides), Shon Feder (Tarides), Sudha Parimala (Tarides),
Ambre Suhamy (Tarides)

## Summary

This document specifies the requirements for dune's tool management system.

<!-- To regenerate TOC:
nix shell --impure --expr 'let pkgs = import (builtins.getFlake "github:NixOS/nixpkgs") {}; in (pkgs.emacs.pkgs.withPackages (ps: [ps.markdown-toc]))' -c emacs --batch --eval "(progn (require 'markdown-toc) (find-file \"doc/dev/tools.md\") (markdown-toc-refresh-toc) (save-buffer))"
-->

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Dune Tools Requirements](#dune-tools-requirements)
  - [Summary](#summary)
  - [How to Read This Document](#how-to-read-this-document)
  - [Terminology](#terminology)
  - [Design principles](#design-principles)
  - [Requirements](#requirements)
    - [1. Installation](#1-installation)
      - [1.1. Generality](#11-generality)
        - [1.2. Scope](#12-scope)
      - [1.2.1. Workspace-local scope](#121-workspace-local-scope)
      - [1.2.2. Dune context scope](#122-dune-context-scope)
      - [1.2.3. System wide scope](#123-system-wide-scope)
      - [1.4. Version specification](#14-version-specification)
        - [1.4.1. Version consistency](#141-version-consistency)
      - [1.5. Clean source tree](#15-clean-source-tree)
      - [1.6. Binary selection](#16-binary-selection)
      - [1.7 By tool name](#17-by-tool-name)
        - [1.7.1 Disambiguation](#171-disambiguation)
      - [1.8 Project dependency tools](#18-project-dependency-tools)
        - [1.9 Discretionary tools](#19-discretionary-tools)
    - [2. Usability](#2-usability)
      - [2.1 Extent of tool management ](#21-extent-of-tool-management)
        - [2.1.1 when package management is enabled in a workspace](#211-when-package-management-is-enabled-in-a-workspace)
        - [2.1.2 when package management is not enabled in a workspace](#212-when-package-management-is-not-enabled-in-a-workspace)
      - [2.2. Shells](#22-shells)
      - [2.3. Programmatic use](#23-programmatic-use)
        - [2.3.1. dune subcommands](#231-dune-subcommands)
          - [2.3.1.1. System PATH fallback](#2311-system-path-fallback)
          - [2.3.1.1.1. When package management is enabled in a workspace](#23111-when-package-management-is-enabled-in-a-workspace)
          - [2.3.1.1.2. When package management is NOT enabled in a workspace ](#23112-when-package-management-is-not-enabled-in-a-workspace)
      - [2.4. Project dependency tools](#24-project-dependency-tools)
    - [3. Dependency and Integration](#3-dependency-and-integration)
      - [3.1 Integration axis](#31-integration-axis)
        - [3.1.2. Tool isolation (I1)](#312-tool-isolation-i1)
        - [3.1.2 Compiler integrations (I2)](#312-compiler-integrations-i2)
          - [3.1.2.1 Identifying compiler integrations](#3121-identifying-compiler-integrations)
          - [3.1.2.2 Handling compiler integration constraints](#3122-handling-compiler-integration-constraints)
      - [4.2 Dependency axis](#42-dependency-axis)
        - [4.1. Discretionary tools (D1)](#41-discretionary-tools-d1)
          - [4.1.1. Not build triggers](#411-not-build-triggers)
          - [4.1.2. Cannot be referenced in build rules TODO](#412-cannot-be-referenced-in-build-rules-todo)
        - [4.2. Qualified dependency tools (D2) TODO](#42-qualified-dependency-tools-d2-todo)
          - [4.2.1. Must be usable as tools](#421-must-be-usable-as-tools)
          - [4.2.1. Must be installable via qualification](#421-must-be-installable-via-qualification)
        - [4.3. Unqualified dependency tools (D3) TODO](#43-unqualified-dependency-tools-d3-todo)
          - [4.3.1. Must be usable as tools TODO](#431-must-be-usable-as-tools-todo)
          - [4.2.1. Must always be installed TODO](#421-must-always-be-installed-todo)
      - [5. UI](#5-ui)
      - [5.1. CLI](#51-cli)
        - [5.1.1. Managing multiple tools](#511-managing-multiple-tools)
      - [5.2. Persistent configuration for discretionary tools](#52-persistent-configuration-for-discretionary-tools)
      - [5.4. Tool references in actions](#54-tool-references-in-actions)
      - [5.5. Legacy migration](#55-legacy-migration)
- [TODO Orphans to put in proper section](#todo-orphans-to-put-in-proper-section)
  - [-](#-)
  - [4. Dog fooding](#4-dog-fooding)
- [TODO (Shon, Ambre, Sudha): Analyze out the axes of use and integration](#todo-shon-ambre-sudha-analyze-out-the-axes-of-use-and-integration)
  - [There are at least 2 axes](#there-are-at-least-2-axes)
    - [Usage categories spectrum:](#usage-categories-spectrum)
    - [Integration requirements spectrum:](#integration-requirements-spectrum)
  - [Some example](#some-example)
  - [Some ideas](#some-ideas)

<!-- markdown-toc end -->

## How to Read This Document

[Terminology](#terminology) defines key terms used throughout the document.
Precise definition of technical terms prevents misunderstandings and certain
classes of requirement error.

[Design principles](#design-principles) specifies the guiding principles
informing the design.

[Requirements](#requirements) defines _what_ capabilities the system must
provide and _why_.  Organized by category. Requirements describe user-facing
behaviour without specifying implementation details. Cross-references point to
the relevant Implementation sections.

## Terminology

- A **tool** is just an executable provided by some opam package. One opam
  package can provide multiple tools.
- A **project dependency** is a component (such as a library or executable)
  required for developing and/or distributing a project. For dune, this
  generally means the component must be provided by a package dependency of some
  package in the project. A component is a dependency if *any* of a project's
  build targets depend upon it, whether or not it is correctly declared as such.
- A dependency is **qualified** when it is only required for a certain category
  of build targets. Common qualification include *test* dependencies (e.g.,
  `alcotest`, `jq`, or `qcheck`) and *generation* dependencies (e.g., `atd`,
  `menhir`, or `ocamlformat`).
- A tool is **discretionary**, relative to a particular project, if it is used
  by some developers of the project, but it is not a *project dependency*.
  Common examples include tools like `ocamllsp`, `utop`.
- To **install** a tool is to install the opam package providing it and making
  the tool's executable available within an environment. The system responsible
  for installing and uninstalling a tool is said to **manage** the tool.
- A **well-formed opam package** specifies all data necessary to install and
  build its provided targets.
- A tool is **installable** if it is part of a well-formed opam package
  obtainable from any source: opam repository, pinning from a source, or defined
  locally.
- **Workspace** means a [dune workspace](https://dune.readthedocs.io/en/latest/explanation/scopes.html)

## Design principles

- **Generality**: Any executable defined in an opam packages can be a tool.
- **Orthogonality**: Dune's tool management functionality should compose with
  other dune functionality with maximum flexibility and consistency, and without
  introducing functional redundancies.
- **Complementarity**: Reciprocal to orthogonality, dune's tool management
  functionality should integrate with and augment its core build functionality
  and nascent package management functionality, and avoid introducing a
  bolted-on subsystem.
  
## Requirements

NOTE: The requirements here should be compatible with the functional
requirements in
[https://ocaml.org/tools/platform-roadmap](https://ocaml.org/tools/platform-roadmap).
However, wherever the roadmap specifies implementation details, we are free to
deviate if needed to better satisfy the requirements, and our task here is not
to realize all of the requirements in the platform roadmap.

### 1. Installation

Users must be able to install tools via Dune.

#### 1.1. Generality

Any installable tool must be supported.

##### 1.2. Scope

The environments within which a tool are available must be scoped.

#### 1.2.1. Workspace-local scope

It must be possible to install tools per-workspace, such that workspaces define
a sub-environment. I.e., each workspace has its own isolated tool installations
that don't affect other workspaces or interfere with software on the host system.

See [Directory structure](./implementation.md#directory-structure) for storage locations.

#### 1.2.2. Dune context scope

It must be possible to install the versions of a tool per-dune context within a
workspace, such that different contexts can use different versions of tools.

TODO: tools as lock dirs to impl doc. In particular, specifying multiple tools
in field in one stanza, to avoid lots of duplication, but mapping this to
separate lock dirs.

<details>
<summary>
Motivation and context
</summary>

This follows the example of `dune pkg`'s `lock_dir` in the context stanza, and
allows developers to set up different tooling configurations.

We have sketched a preliminary design that considers `tools` stanzas as
specialized form of `lock_dir` stanza that inherits the fields of the active
`lock_dir` in a context if no overriding fields are specified.

</details>

#### 1.2.3. System wide scope

Users should be able to install tools in a way that allows them to be used in the
system-wide environment (e.g., outside of any particular sandbox).

*NOTE:* This does not dictate that dune must maintain the equivalent of default
switches, or predetermine any other implementation choice. But the support for
tool management must be designed in way that makes it simple and reliable to use
installed tools outside of a project sandbox (e.g., by adding the location of a
directory of binaries in a workspace to their system `PATH`, or some other
means).

<details>
<summary>
Motivation and context
</summary>

Related issues:

- [dune#12107 pkg: installation of packages that can be used
  system-wide](https://github.com/ocaml/dune/issues/12107)
  - Even if we don't support this in the first version, the design should not
  prevent this.

</details>

#### 1.4. Version specification

Users must be able to specify the version of tools to be installed via:

- CLI arguments
- Declarative configuration (i.e. dune stanzas)
- Tool-specific configuration files (e.g., `.ocamlformat`)

See [Version syntax](./implementation.md#version-syntax) for CLI syntax and
[The `(tool)` stanza](./implementation.md#the-tool-stanza) for declarative configuration.

##### 1.4.1. Version consistency

The versions of installed tools must remain consistent, accounting for all
configuration sources. E.g., consider an apparent conflict, such as a stanza
specifying `(= 0.26.2)` but a CLI input specifying `0.27.0`, or `.ocamlformat` says
`version=0.26.2` but the stanza says `(= 0.27.0)`: in such cases, a
consistent outcome must be derived. E.g., this could be achieved by having the
CLI input being used to update the config file, or simply by raising an error.
But it must not allow for a version to be installed that leads to inconsistent
version specifications.

#### 1.5. Clean source tree

Tool lock directories and built artifacts must not pollute the source tree, to
ensure that they do not inadvertently picked up in version control or otherwise
create needless noise for users.

**Motivation**: This is a common complaint from users and is one such way to
solve the issue. Tools like `uv` handle this differently by having a global place.
Due to our compiler matching semantics it makes more sense for workspace level
and becomes fast with full caching.

#### 1.6. Binary selection

When a package provides multiple tools, users must be able to specify a subset
for installation. When a package providing tools is installed without
qualification, all provided tools must be installed. As a special case, when a
package provides a single binary, it will be installed without needing to qualify.

See [The `(tool)` stanza](./implementation.md#the-tool-stanza) and [CLI
commands](./implementation.md#cli-commands) for syntax.

**Motivation**: This is necessitated by the fact that the relation between opam
packages and tools is one-to-many: a single package can provide multiple
executables. As a result, it becomes necessary to only install a preferred
subset of the provided tools. E.g., `js_of_ocaml-compiler` provides
`js_of_ocaml`, `jsoo_minify`, and `jsoo_listunits`.

#### 1.7 By tool name

Users should be able to install tools based on the name of the tool without
considering the package that provides it.

CR Shon: because this will require changes to the opam repo to be effective,
this is a *should* rather than a *must* at the moment, and may not be achievable
in the first iteration of the redisign.

##### 1.7.1 Disambiguation

If multiple packages provide the tools with the same name, and a user requests installation, dune should offer disambiguation.

CR Shon: what do we do in dune package management if two packages provide the
same executable? E.g., perhaps you want to use package a for tool a' and b for
b', but they both also provide executables named `c`?

#### 1.8 Project dependency tools

Tools that are *project dependencies* specified as appropriately qualified
dependencies in the `dune-project` file, must be installable via installation
targets reflecting the qualification, as well as thru the build targets that
require them.

To illustrate, this could be thru some sort of qualification to the tools
command like `dune tools install :with-test :with-dev-setup`.

<details>
<summary>
Motivation and context
</summary>

Related issues:

- [dune#12135 dune tools setup to install :with-dev-setup
  deps](https://github.com/ocaml/dune/issues/12135)
  - Should the :with-dev-setup qualifier be used by dev-tools to install?

</details>

##### 1.9 Discretionary tools

It must be possible to install discretionary tools without incorrectly
specifying them as if they were project dependencies. (E.g., thru a new `tool`
stanza or a CLI that updates some data stored in the workspace or `_build`
directory).

<details>
<summary>
Motivation and context
</summary>

Related issues:

- [dune#12913 pkg: general support for installing
  tools](https://github.com/ocaml/dune/issues/12913)
  - A tool can be any package with a binary.

</details>

### 2. Usability

Users must be able to run tools installed by Dune.

#### 2.1 Extent of tool management 

The extent to which dune enforces management of tools within a workspace should
depend on whether or not package management is enabled in the workspace.

##### 2.1.1 when package management is enabled in a workspace

When users enable dune package management in a workspace, all *tools* used in the
workspace (in the precise sense defined in the [terminology](#terminology))
should be managed dune, to the extent that dune can reasonably enforce this.
Enforcing this assumption allows dune to offer users improved guarantees about
the cohesiveness and interoperability of the provided tools.

This does *not* entail that dune should mask or redact data from the `PATH` or
otherwise attempt to filter out a users ambient environment. But it should
provide pragmatic measures to support users by enforcing this behavior where
feasible.

##### 2.1.2 when package management is not enabled in a workspace

When users have not enabled dune package management in a workspace, they must be
able to use *tools* managed dune, but they should still be able to use tools
installed by opam (or other possible package managers) in all operations of
dune.

#### 2.2. Shells

Users must be able to run tools by invoking them directly in any shell (e.g., bash).

See [CLI commands](./implementation.md#cli-commands) for invocation syntax.

<details>
<summary>
Motivation and context
</summary>

Related issues:

- [dune#12975 running dune tools exec <p> when p is not already installed as a dev
  tool should suggest users run dune tools install <p>](https://github.com/ocaml/dune/issues/12975)
  - The error can be something like
```
dune tools exec merlin
  Error: The tool merlin is not installed
  Hint: Try 'dune tools install merlin'
```

</details>

#### 2.3. Programmatic use

Programs (e.g., editor plugins) must be able to find and run installed tools via
a single, transparent mechanism. (E.g., this could be implemented by any of the
following; the `dune tools env` equivalent to `opam env` or, `dune tools path`
that prints the paths to the binaries,  by adding a single directory of
executables to the lookup path, printing the path locations, or some other
means).

See [CLI commands](./implementation.md#cli-commands) for the discovery interface.

##### 2.3.1. dune subcommands

As a special case, dune subcommands (e.g., `dune fmt` or `dune utop`) that
invoke external tools must be able to use tools managed by `dune tools`, when
they are available.

###### 2.3.1.1. System PATH fallback

The extent to which dune should allow its subcommands to fallback to the system
`PATH` when looking up required binaries depends on whether or not the workspace
has enabled package management, as dictated by
[2.1](#2-1-extent-of-tool-management).

###### 2.3.1.1.1. When package management is enabled in a workspace

When package management is enabled in a workspace but a required tool is not
installed, dune subcommands must produce a clear user error explaining to
users that the tool is not available and directing them to install it as a dune
managed tool.

<details>
<summary>
Motivation and context
</summary>

Related issues:

- [dune#10688 pkg: avoid dune fmt capturing ocamlformat from the
  PATH](https://github.com/ocaml/dune/issues/10688)
  - Install tools automatically?
  - How would having two different package managers side by side work? (In this
  case, Dune package and OPAM)
  - Good to maintain compatibility with OPAM - but the tradeoff is the amount of
  work needed to achieve it.

</details>

###### 2.3.1.1.2. When package management is NOT enabled in a workspace 

When package management is enabled in a workspace and a tool is not installed
(e.g., `.ocamlformat`), dune subcommands (such as `dune fmt`, `dune build @doc`,
or `dune utop`) should fall back to the system PATH.

**Note** This is motivated by integration with editor developers who would like
a single point of truth for running tools, and for dune to handle it. This would
mean opam users can continue to use dune in which ever way they please and the
editors will not have to care.

#### 2.4. Project dependency tools

When a tool is installed as a *project dependency* (under any qualification),
users must be able to execute the tool thru all the same mechanism that are
provided for executing discretionary tools. E.g., a mechanism like `dune env`
that would make the path to dune-managed binaries available must include both
discretionary tools and project dependency tools in the path.

### 3. Dependency and Integration

Tools are depended on in numerous ways to build and develop projects and they
require different levels of integration and interdependence with the other
components (tools, libraries, or other artifacts) of a workspace. Dune must be
able to manage all installable tools across this variety, otherwise the user
experience of tool management will feel inconsistent and irregular, and users
will inevitably find certain subsets of tools unavailable or requiring
unexpected workarounds creating a fragmented and awkward user experience.

We can further refine this requirement along two axes.

#### 3.1 Integration axis

Tools lie along a spectrum of integration requirements with other dependencies
in the project, which we can indicate with tree points:

- I1: At the minimum extreme, some tools can be solved and built in complete
  isolation from the rest of the project they are used in. Tools of this sort
  include ocamlformat, ocp-indent, dune-release, and opam-publish.

- I2: In the midpoint, some tools must integrate with a subset of a project's
  dependencies. The most common examples in this space are tools that must
  integrate with the compiler, such as ocamllsp, odoc, or merlin, but don't need
  to integrate with other libraries in the workspace. Some tools also integrate
  only with select components other than the compiler, such as menhir and atd.
  These tools require integration with the particular version of their runtime
  libraries in certain modes of use, but don't require integrations otherwise,
  and since the tools themselves have a wired dependency cone than the runtime
  libraries, it could sometimes be helpful to bud the executables in a separate
  dependency context, pinned only to the needed runtime library version (e.g.,
  to avoid conflicts over a CLI parser library).

- I3: At the maximum extreme, some tools could require be built within the
  entire dependency context of the project. We are not aware of any tools that
  require this currently, but we can consider utop is an illustrative example,
  since its own dependencies (such as `lwt`, `xdg`, and `logs`) need to
  integrate with the versions in the environment it is installed in, and it
  cannot load code into the top level that would require differing on these
  versions.

Tools lying along I1 and I2 must be supported with elegant solutions where
coupling is required, and the most possible orthogonality in features when it is
not.

##### 3.1.1. Tool isolation (I1)

Unnecessary coupling between tools and other dependencies of a project should
not be introduced. When an *installable* tool sits at I1 on the integration
axis, dune should be able to install it in a workspace, without regard to any
possible conflicts with *project dependencies*. By definition, tools at I1 have
no integration requirements with other components in a project, so there are no
grounds for such conflict. This property *should* hold rather than *must*,
because it may be infeasible and unnecessary to implement this requirement for
tools that are project dependencies.

However, for discretionary tools at I1 this property *must* be guaranteed.

<details>
<summary>
Motivation and context
</summary>

See [Directory structure](./implementation.md#directory-structure) for lock
directory locations.

</details>

###### 3.1.1.1 Optimal builds

Dune should not needless compile or rebuild dependencies that can be shared
without conflict. E.g., if the needed version of a discretionary tool
can be installed by reusing the compiler version already installed for a
workspace, or by pulling it from the cache, this should be preferred over
rebuilding the tool or its dependencies from scratch.

##### 3.1.2 Compiler integrations (I2)

TODO: `integrates_with compiler|minhirLib` ?
TODO: Need to spec more general integration requirements, with compiler as
special case.

Tools that integrate with the compiler hold a special status among tools, as a
result of their ubiquitous use and the special position of the compiler itself.
The most widely used tools of this sort are ocamllsp and odoc. Dune must provide
robust, intuitive, and flexible support for managing these tools.


<details>
<summary>
Motivation and context
</summary>

See [Compiler matching](./implementation.md#compiler-matching) for the detection
algorithm.

</details>

###### 3.1.2.1 Identifying compiler integrations

It must be possible to identify tools that require integration with the compiler
version in a workspace and install them appropriately (e.g., via an explicit
designation provided by the user, a list of known tools, some data available in
the package definition, or any other means).

###### 3.1.2.2 Handling compiler integration constraints

When a user has specified the intent to install a compiler integration that is
incompatible with the compiler version installed in the workspace, dune must
handle the conflict gracefully and with clear guidance to the user. 

This could mean automatically downgrading the compiler (if permitted by
the constraints), but a user error with clear instructions is probably just as
effectively, less surprising, and much easier to implement. E.g., "tool t cannot
be installed at version v because it requires compiler <= n, but compiler m is
currently installed. Add the following qualified constraint to your project
dependencies: `(ocaml (and (>= 5.1 (or (not :with-dev-setup) (< 5.5))))`" etc.

Regardless of the implementation approach taken, it must be possible to
constrain the compiler versions to work with the desired integrations during
development without those constraints polluting the constraints of packages in
the workspace when they are installed.


#### 4.2 Dependency axis

Tools lie along a spectrum of dependency status (i.e., to what extent they are
dependencies for the project) which we can indicate with these tree points:

- D1. At the minimum extreme, *discretionary tools* are not project dependencies
  at all. Rather, they are tools we want to install and run on an ad hoc basis,
  for use as developers. E.g., `ocamllsp` or `ocamlgrep`.
- D2. In the midpoint, *qualified project dependency* tools are used in rules associated
  with a build alias, but not as part of the required package dependencies to
  install any packages. Example include any tools that are qualified in
  opam packages with the `with-test`, or `with-doc` filters.
- D3. At the maximum extreme, unqualified *project dependency* tools are
 required for an installation build of packages in the project, and need to be
 specified as package dependencies unconditionally.E.g., `menhir` or `atd` when
 used as part of the project build (by contrast, if these are used only for code
 generation, they could be qualified project dependencies behind a hypothetical
 `with-gen` filter).

##### 4.1. Discretionary tools (D1)

###### 4.1.1. Not build triggers

Locking or adding tools in at D1 level tools must not trigger project builds.

###### 4.1.2. Cannot be referenced in build rules TODO

##### 4.2. Qualified dependency tools (D2) TODO

###### 4.2.1. Must be usable as tools

###### 4.2.1. Must be installable via qualification

##### 4.3. Unqualified dependency tools (D3) TODO

###### 4.3.1. Must be usable as tools TODO

###### 4.2.1. Must always be installed TODO

#### 5. UI

#### 5.1. CLI

Users must be able to manage tools using CLI commands:

- Add/lock individual tools to the workspace
- Run tools (building if needed)
- List locked tools and versions
- Remove tools
- Discover paths to tool executables

TODO: we may want a req. that `dune clean` does not wipe out tools

CLI-added tools persist until `dune clean` or `dune tools remove`, but are not
reproducible across clean builds. See [Batch
operations](./implementation.md#batch-operations) for batch commands.

##### 5.1.1. Managing multiple tools

It must be possible to change (e.g., update, install, remove) the installation
of all configured tools by issuing a single command.

<details>
<summary>
Motivation and context
</summary>

We must not require users to do the tedious work of running the same command
over and over to install a set of tools. How this is addressed is left as an
implementation detail. E.g., it could mean supporting `dune tools install a b c`
or just having a command that installs all configured tools (see 5.2) in one
command.

Related issues:

- [dune#12557 dune tools install should take multiple package
  arguments](https://github.com/ocaml/dune/issues/12557)
  - Discussed in the implementation spec. See [./implementation.md#batch-operations](./implementation.md#batch-operations)

</details>


#### 5.2. Persistent configuration for discretionary tools

Users must be able to declare discretionary tools in the workspace
configuration. 

<details>
<summary>
Motivation and context
</summary>

This allows users to share tooling configurations between users and across fresh
project setups as part of a reusable ad hoc dev tool setup.

- Declaration is version-controlled and shared with collaborators
- CI and fresh checkouts get the same tools

See [The `(tool)` stanza](./implementation.md#the-tool-stanza) for proposed syntax.

<details>

#### 5.4. Tool references in actions

CR Shon: anything referenced by an action is U2 or greater on the "integration
requirement spectrum".

Build actions (both user-written rules and dune's internal rules like
formatting) must be able to reference the installed tools. E.g., via
`%{bin:...}` pforms, the base executable name, (or by some new mechanism, if
there is need for it).

#### 5.5. Legacy migration

TODO: is this just a task for our project, or is it actually a requirement? Is the requirement about backward compat.

The legacy `.dev-tools.locks/` system must be removed and replaced by this
design. Entailing updating all documentation and communicating needed users
interventions, as necessary.

<!-- CR-soon Alizter: Detail the migration story: -->

<!-- - Which CLI commands stay (with same or changed behavior)? -->
<!-- - Which CLI commands are removed? -->
<!-- - What happens to existing `.dev-tools.locks/` directories? -->
<!-- - User-facing migration guide (re-add tools via `dune tools add`) -->


# TODO Orphans to put in proper section


#### N. Watch mode integration

Tool operations (e.g., `dune tools add`, `run`, etc.) must work correctly when a
watch server is running (`dune build -w`). Rather than directly manipulating
lock directories, tool commands should coordinate with the watch server via RPC
to avoid races and ensure the server picks up newly added tools.

CR-soon Alizter: Specify the RPC protocol for tool operations. What messages are
needed? How does the watch server respond to tool additions?

CR-soon Alizter: Document concurrent access behavior. What happens if two
terminals run `dune tools add` simultaneously? Or `add` while `run` is building?

#### 4. Dog fooding

Tools must work in the dune repository itself. Dune developers should be able to
run `dune tools add ocamlformat` and `dune tools add ocaml-lsp-server` when
working on dune.

This is enabled by the orthogonality design (3.2): tools are solved and built
independently from project dependencies, so they don't require a working project
lock directory.


# TODO (Shon, Ambre, Sudha): Analyze out the axes of use and integration

This is about "the tool-dependency spectrum": most tools do not fit into the
binary between:

- an adhoc binary a person wants to run a few times or
- a library dependency required for a project

Most tools actually have fall on a spectrum with varying levels of integration
requirements, and various kinds of usage within the SDLC.

Three ways of invoking a tool:

- shell environment: (dune tools env)
- sub commands: dune fmt / dune build @doc
- dedicated dune runner subcommands: dune tools run/exec

How do tools that are required for development currently interact with the
 current deps? E.g., from opam, `(deps (odoc (and 2.1.1 :with-dev-setup)))`

## There are at least 2 axes

Each a spectrum, but we can sketch its shape using the two extremes and the
mixed case.

### Usage categories spectrum:

- U1. tools we want to install and run on an adhoc basis: dune tools install og;
 og foo `dune build @tools` `dune build @editors-tools` :tools `dune pkg lock`.
- U2. tools we want to use as part of a build alias, but not as part of the
 package dependencies (for tests, for build docs, fmt, could be geneneralized to
 any build alias): with-doc, with-test, with-dev-setup
- U3. tools that are required for a build (e.g., menhir or atd), and just need
 to be a package dependency unconditionally (but which users will also want to
 be able to execute)!

### Integration requirements spectrum:

- I1. tools that must integrate with all other packages in a solution (e.g.,
 because of shared libraries)
- I2. tools that must integrate just with the compiler (aka "compiler
 integrations" or "toolchain utilities")
- I3. tools that do not need to integrate with anything, and can be installed in
 completely isolated environments. 

## Some example

pkg foo
 deps A.1

cmdliner   --with-test
 deps A.1
 
ocamllsp # exectable, but it hast to have library like integration (with compiler)
 
ocamlformat
 deps A.2

atd -> foo_j.ml (* as a fixuter json serde *)
  deps A.3
  
## Some ideas

Could consider a qualifier on executable packages

- like "disjoint" or "sandboxed": but would have to be only for executables 
- :exec-only, in which case (so long as not compiler integration!) can always be sandboxed

Libs vs. executables.

TODO: Consider use cases in many different forms and classify.

- Menhir
- Ocsigen
- Coq/Fstar
- grep tools
