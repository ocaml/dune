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
        - [3.1.1. Tool isolation (I1)](#311-tool-isolation-i1)
          - [3.1.1.1. Optimal builds](#3111-optimal-builds)
        - [3.1.2. Tool integration (I2)](#312-tool-integration-i2)
          - [3.1.2.1. Respecting integration constraints](#3121-respecting-integration-constraints)
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

#### 2.4. Dog fooding

Tools must work in the dune repository itself. Dune developers should be able to
run `dune tools add ocamlformat` and `dune tools add ocaml-lsp-server` when
working on dune.

This is enabled by the orthogonality design (3.2): tools are solved and built
independently from project dependencies, so they don't require a working project
lock directory.

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

Unnecessary coupling between tools and other dependencies of a project _should_
not be introduced. When an *installable* tool sits at I1 on the integration
axis, dune should be able to install it in a workspace, without regard to any
possible conflicts with *project dependencies*. By definition, tools at I1 have
no integration requirements with other components in a project, so there are no
grounds for such conflict. This property *should* hold rather than *must*,
because it may be infeasible and unnecessary to implement this requirement for
tools that are project dependencies.

However, for *discretionary tools* at I1 this property *must* be guaranteed:
dune must support installation of any installable, discretionary tool at I1,
without regard to possible dependency conflicts with project dependencies.

<details>
<summary>
Motivation and context
</summary>

To indicate just one dimension of the motivations here, consider this small set
of arbitrarily chosen packages providing I1 tools, which are all currently
published to opam and all currently incompatible with ocaml 5.5 (due to direct
or transitive constraints):

- [cca](https://ocaml.org/p/cca/latest): "A framework for differential source
  code analyses" 
- [comby](https://ocaml.org/p/comby/latest): "A tool for structural code search
  and replace that supports ~every language" 
- [crs](https://ocaml.org/p/crs/latest): "A tool for managing inline review
  comments embedded in source code"
- [depgraph](https://ocaml.org/p/depgraph/latest): "dot graphs out of ocamldep
  output"
- [electrod](https://ocaml.org/p/electrod/latest): "Formal analysis for the
  Electrod formal pivot language"
- [facteur](https://ocaml.org/p/facteur/latest): "Tool to send an email"
- [pfff](https://ocaml.org/p/pfff/latest): "Tools and APIs for program analysis,
  code visualization, refactoring"
- [pgn_to_tex](https://ocaml.org/p/pgn_to_tex/latest): "A chess PGN to TeX conversion tool"
- [rdr](https://ocaml.org/p/rdr/latest): "Rdr is a cross-platform binary
  analysis and reverse engineering tool, utilizing a unique symbol map for
  global analysis."

Without support for dependency isolation, users would be forced to chose between
avoiding recent compiler versions or making use of these (and many other)
available tools. This would violate the design principles of orthogonality and
generality, and yield and necessarily limited usability.

See  [Directory structure](./implementation.md#directory-structure) for proposed
lock directory locations.

</details>

###### 3.1.1.1. Optimal builds

Dune should not needless compile or rebuild dependencies that can be shared
without conflict. E.g., if the needed version of an I1 discretionary tool can be
installed by reusing the compiler version already used in the workspace, or by
pulling it from the shared cache, this should be preferred over rebuilding the
tool or its dependencies from scratch.

##### 3.1.2. Tool integration (I2)

When a tool requires integration with other components in the workspace at their
specific version, it must be possible to specify this requirement and have the
solution, build, and install enable that needed integration.

Tools that integrate with the compiler are a very special case of I2 tools
requiring this need, and they have an important status among other tools because
some are used ubiquitous and because he compiler itself has a special position
in the dependency tree of any OCaml project. The most widely used tools of this
sort are ocamllsp and odoc. Dune must provide robust, intuitive, and flexible
support for managing these tools.

<details>
<summary>
Motivation and context
</summary>

One way to address this may be thru the an equivalent of the `constraints` field
in the `lock_dir` stanza, where an absent dep-specification indicated the need
to use the same version in the context's active `lock_dir`. E.g., as

```
(tools
  ((ocamllsp (from ocaml-lsp-server))
    utop)
  (constraints ocaml))
```

Which would require that the named tools be constrained on the version of ocaml
used in the workspace.

A special purpose field could also be introduced for this purpose.

We can also consider data added to opam files that allows this requirement to be
specified for provided tools at the package data level, instead of forcing this
upon consuming users.

See [Compiler matching](./implementation.md#compiler-matching) for the detection
algorithm.

</details>

###### 3.1.2.1. Respecting integration constraints

When a user has specified the intent to install an I2 tool that is incompatible
with the required packages already installed in the workspace, dune must handle
the conflict gracefully an with clear guidance to the user.

E.g., if a user tries to install the compiler integration `ocamllsp` and not
version is compatible with the compiler version already installed in the
workspace, dune must report and/or solve the conflict cleanly.

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

Note that, since tools like `utop`, `odoc`, and `ocamlformat` have dedicated
builtin rules from dune, they are technically not D1.

##### 4.1. Discretionary tools (D1)

Dune must be able to manage discretionary tools.

###### 4.1.1. Not build triggers

Locking or adding discretionary tools must not trigger project builds.

###### 4.1.2. Cannot be referenced in build rules

Discretionary tools managed by dune must not be referenced in user build rules: dune
must not add these binaries to its path when running rules, and must not
resolve `%{bin:...}` forms that attempt to reference them. 

By definition, tools referenced in rules are at least D2.

###### 4.1.2.1. Useful guidance on invalid reference in rules

When a discretionary tool is configured and an invalid reference to it is found
in a build rule, dune should report an error with clear guidance to users,
advising them to move the tool configuration into the appropriate package
dependency.

###### 4.1.3. Subset of D2 and D3 tools functionality

The functionality of discretionary, D1 tools must a strict subset of the functionality over
D2 and D3 project dependency tools: anything you can do with 

##### 4.2. Qualified project dependency tools (D2)

Qualified project dependency tools must be installable based on the existing
package dependency configuration mechanism. This configuration may be extended
to support tool-specific configuration when the need and means to achieve that
is known.

<details>
<summary>
Motivation and context
</summary>

When tools are invoked in build rules, they are dependencies of a project, since
the full breadth of the project cannot be built without installing the tool. We
have an existing mechanism for declaring dependencies for installation, and so
based on our principles of orthogonality and complementarity, we must not
introduce redundant ways of solving the same problem that is already accounted
for. Doing so would not only be inelegant, but lead to user confusion and
package specification fragmentation, since the existing systems is also
supported by opam.

</details>

###### 4.2.1. Usable as tools

Qualified project dependency tools must be usable in exactly the same ways as
discretionary, D1 tools. E.g., if D1 tools can be run via a command like `dune
tools exec ...`  then so too must be D2 tools. However, the implementation is
not prescribed by the requirement, and we could instead permit D1 tools to be
run via the existing `dune exec ...` subcommand, providing a simpler and more
intuitive interface to users than requiring to separate subcommands.

###### 4.2.2. Installable via qualification

If a set of tools are qualified with a filter such as `:with-test` or
`:with-doc`, it must be possible to install just that set together (in addition
to the unqualified dependencies).

###### 4.2.3. Builtin D2 tools

A select subset of keystone tools are treated by dune as builtin qualified
dependencies, including `odoc`, `utop`, and `ocamlformat` as the most widely
used. These should be treated as if they have qualified dependencies built in,
with further constraint or specification of how to install them available as a
user override on top of the default configuration.

##### 4.3. Unqualified dependency tools (D3) TODO

###### 4.3.1. Usable as tools

As with 4.2.1, unqualified project dependency tools must be usable in exactly
the same ways as D1 and D2 tools.

###### 4.2.1. Installed on build

Unqualified tools must be installed when a project is built.

(Already satisfied by the current implementation of package managemet.)

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

It must be possible to change (e.g., update, install, or remove) the
installation of a set of configured tools by issuing a single command.

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

##### 5.1.2. Watch mode integration

Tool operations (e.g., `dune tools add`, `run`, etc.) must work correctly when a
watch server is running (`dune build -w`). Rather than directly manipulating
lock directories, tool commands should coordinate with the watch server via RPC
to avoid races and ensure the server picks up newly added tools.

##### 5.1.3 Avoid invocation collisions

When a `dune tools` command is run that depends on the state of the workspace,
it should not interfere with concurrent running dune commands or lead to invalid
results or states. This may just mean refusing to run if the build directory is
locked, cooperating to sequence requests, or something else.

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

