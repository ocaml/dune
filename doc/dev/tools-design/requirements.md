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

- [Tools Requirements](#tools-requirements)
  - [Summary](#summary)
  - [How to Read This Document](#how-to-read-this-document)
  - [Terminology](#terminology)
  - [Design principles](#design-principles)
  - [Requirements](#requirements)
    - [1. Installation](#1-installation)
      - [1.1. Generality](#11-generality)
      - [1.2. Workspace-local](#12-workspace-local)
      - [1.3. System wide](#13-system-wide)
      - [1.4. Version specification](#14-version-specification)
        - [1.4.1. Version consistency](#141-version-consistency)
      - [1.5. Multi-version support](#15-multi-version-support)
      - [1.5. Clean source tree](#15-clean-source-tree)
      - [1.6. Binary selection](#16-binary-selection)
    - [2. Usability](#2-usability)
      - [2.1. Shells](#21-shells)
      - [2.2. Version specification TODO](#22-version-specification-todo)
      - [2.3. Programmatic use](#23-programmatic-use)
        - [2.3.1 dune subcommands](#231-dune-subcommands)
      - [2.5. System PATH fallback](#25-system-path-fallback)
      - [2.6. Editor integration](#26-editor-integration)
    - [3. Dependency interactions](#3-dependency-interactions)
      - [3.1. Compiler compatibility](#31-compiler-compatibility)
      - [3.2. Dependency isolation](#32-dependency-isolation)
      - [3.3. No build triggers](#33-no-build-triggers)
      - [3.4. Watch mode integration](#34-watch-mode-integration)
      - [3.5. Dog fooding](#35-dog-fooding)
    - [4. UI](#4-ui)
      - [4.1. CLI](#41-cli)
      - [4.2. Persistent configuration](#42-persistent-configuration)
    - [5. Dune Integration](#5-dune-integration)
      - [5.1. Format rules](#51-format-rules)
      - [5.2. Documentation rules (`dune build @doc`, `dune ocaml doc`)](#52-documentation-rules-dune-build-doc-dune-ocaml-doc)
      - [5.3. REPL (`dune utop`, `dune ocaml utop`)](#53-repl-dune-utop-dune-ocaml-utop)
      - [5.4. Tool references in actions](#54-tool-references-in-actions)
      - [5.5. Legacy migration](#55-legacy-migration)

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
  required for developing and/or distributing a project. In the context of dune,
  we know a component is a dependency if *any* of a project's build targets
  depend upon it.
- A dependency is **qualified** when it is only required for a certain category
  of build targets. Common qualification include *test* dependencies (e.g.,
  `alcotest`, `jq`, or `qcheck`) and *generation* dependencies (e.g., `atd`,
  `menhir`, or `ocamlformat`).
- A tool is **discretionary**, relative to a particular project, if it is used
  by some developers of the project, but it is not a *project dependency*.
  Common examples include tools like `ocamllsp`, `utop`.
- To **install** a tool is to install the opam package providing it and making
  the tool's executable available within an environment.
- A **well-formed opam package** specifies all data necessary to install and
  build its provided targets.
- A tool is **installable** if it is part of a well-formed opam package obtainable
 from any source: opam repository, pinning from a source, or defined locally.
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
that don't affect other workspaces or interfere the system.

See [Directory structure](./implementation.md#directory-structure) for storage locations.

#### 1.2.2. Dune context scope

CR Shon: Is this actually a good idea? How do tools interact with contexts?

It must be possible to install the versions of a tool per-dune context within a
workspace, such that different contexts can use different versions of tools.

This follows the example of `dune pkg`'s `lock_dir` in the context stanza, and
allows developers to set up different tooling configurations (CR. What for?).

#### 1.2.3. System wide scope

Users should be able to install tools in a way that allows them to be used in the
system-wide environment (e.g., outside of any particular sandbox).

*NOTE:* This does not dictate that dune must maintain the equivalent of
default switches, or predetermine any other implementation choice. But the
support for tool management must be designed in way that makes it simple and
reliable to use installed tools outside of a project sandbox (e.g., by
adding the location of a directory of binaries to their `PATH` or some other
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

#### 2.1. Shells

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

#### 2.2. Programmatic use

Programs (e.g., editor plugins) must be able to find and run installed tools via
a single, transparent mechanism. (E.g., this could be implemented by any of the
following; the `dune tools env` equivalent to `opam env` or, `dune tools path`
that prints the paths to the binaries,  by adding a single directory of
executables to the lookup path, printing the path locations, or some other
means).

See [CLI commands](./implementation.md#cli-commands) for the discovery interface.

##### 2.2.1 dune subcommands

As a special case, dune subcommands (e.g., `dune fmt` or `dune utop`) that
invoke external tools must be able to use tools managed by `dune tools`, with
fallback to executables available on the system path.

###### 2.2.1.1 System PATH fallback

When a tool is not installed (e.g., `.ocamlformat`), dune subcommands (such as
`dune fmt`, `dune build @doc`, or `dune utop`) should fall back to the system
PATH.

**Note** This is motivated by integration with editor developers who would like
a single point of truth for running tools, and for dune to handle it. This would
mean opam users can continue to use dune in which ever way they please and the
editors will not have to care.

CR Sudha247: do we need to split this into cases for when pkg is enabled and
disabled? If pkg is enabled, we don't provide system fallback as it could be
confusing to users if aliases just pickup binaries from stale OPAM switches.

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

#### 2.4 Orthogonal execution

Running dune tools outside of a build (by any means) should not interfere with
other concurrent dune operations (e.g., `dune build -w`).

#### 2.5 Project dependency tools

When a tool is a *project dependency* (under any qualification), users must
be able to execute the tool.

### 3. Integration spectrum

Tools lie along a spectrum of integration requirements with other dependencies
in the project, which we can indicate with these tree points:

- I1: At the minimum extreme, some tools can be solved and built in complete
  isolation from the rest of the project they are used in. E.g., ocamlformat.
- I2: In the midpoint, some tools must integrate with a subset of a project's
  dependencies. E.g., ocamllsp must use the same compiler version.
- I3: At the maximum extreme, some tools must be built within the entire
  dependency context of the project. E.g., menhir when used together with its
  runtime libraries, to ensure that the generated parser code is compatible with
  the runtime library.

Tools lying along the entire spectrum must be supported elegant solutions where
coupling is required, and the most possible orthogonality in features when it is not.

#### 3.1. Compiler compatibility

TODO: need to handle tools that are compiler integrations, and want to have an "optimal" dependency environment.
E.g., installing the latest version of a tool, with whatever compiler version it may need, or installing a tool that is not compatible with your env compiler.

By default, tools are built with a compiler matching the environment. This
ensures tools like ocamllsp can read project build artifacts correctly.

Users should be able to opt out of compiler matching per-tool for tools that
don't need it (e.g., formatters that only parse source text).

CR-soon Alizter: The utility of opting out seems debatable, but is part of the
package-tool continuum approach.

See [Compiler matching](./implementation.md#compiler-matching) for the detection
algorithm.

CR-someday Alizter: Reverse influence - could tools constrain the project? E.g.,
if ocamllsp isn't available for OCaml 5.3 yet, a user might want their tool
requirements to influence which compiler version they use. Currently tools are
fully isolated and don't affect project solving. This would be a significant
departure from the orthogonality principle.

#### 3.2. Dependency isolation

The dependencies of each tool should be solved independently with its own lock
directory. Tool dependencies do not affect the project's `dune.lock`, and vice
versa.

See [Directory structure](./implementation.md#directory-structure) for lock
directory locations.


### 4. Dependency spectrum

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

#### 4.1. Discretionary tools (D1)

##### 4.1.1.  Not build triggers

Locking or adding tools in at D1 level tools must not trigger project builds.

##### 4.1.2. Cannot be referenced in build rules TODO

#### 4.2. Qualified dependency tools (D2) TODO

##### 4.2.1. Must be usable as tools

##### 4.2.1. Must be installable via qualification

#### 4.3. Unqualified dependency tools (D3) TODO

##### 4.3.1. Must be usable as tools TODO

##### 4.2.1. Must always be installed TODO

### 5. UI

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
