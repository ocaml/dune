# Tools Requirements

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

<!-- TODO Should these three sections be moved into implementation doc? -->

[Implementation](./implementation.md) details _how_ the system implements the requirements:
stanza syntax, CLI commands, version resolution algorithm, and directory
structure. Each section notes which requirements it implements. Open questions
are marked where decisions are pending.

[Relationship to Package Management](./implementation.md#relationship-to-package-management)
explains the orthogonality principle and how tools differ from project
dependencies.

[Comparison with Other Tools](./implementation.md#comparison-with-other-tools)
analyzes how `uv`,`cargo`, `cargo-run-bin`, and `npm` handle tool management,
informing our design decisions.

## Terminology

- A **tool** is just an executable provided by some opam package. One opam package can provide multiple tools.
- To **install** a tool is to make the executable available within an environment.
- A **well-formed opam package** specifies all data necessary to install and
  build its provided targets.
- A tool is **installable** if it is part of a well-formed opam package obtainable
 from any source: opam repository, pinning from a source, or defined locally.
- **Workspace** means a [dune workspace](https://dune.readthedocs.io/en/latest/explanation/scopes.html)

## Design principles

- **Generality**: Any executable defined in an opam packages can be a tool.
- **Orthogonality**: Tool management should not interfere with other systems,
  including the management of other tools.

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
allows developers to set up different tooling configurations (What for?).

#### 1.2.3. System wide scope

Users should be able to install tools in a way that allows them to be used in the
system-wide environment (e.g., outside of any particular sandbox).

*NOTE:* This does not dictate that dune must maintain the equivalent of
default switches, or predetermine any other implementation choice. But the
support for tool management must be designed in way that makes it simple and
reliable to use installed tools outside of a project sandbox (e.g., by
adding the location of a directory of binaries to their `PATH` or some other
means).

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

### 2. Usability

Users must be able to run tools installed by Dune.

#### 2.1. Shells

Users must be able to run tools by invoking them directly in any shell (e.g., bash).

See [CLI commands](./implementation.md#cli-commands) for invocation syntax.

#### 2.2. Programmatic use

Programs (e.g., editor plugins) must be able to find and run installed tools via
a single, transparent mechanism. (E.g., this could be implemented by any of the
following; the `dune tools env` equivalent to `opam env` or, `dune tools path`
that prints the paths to the binaries,  by adding a single directory of
executables to the lookup path, printing the path locations, or some other
means).

See [CLI commands](./implementation.md#cli-commands) for the discovery interface.

##### 2.2.1 dune subcommands

As a special case, dune subcommands (e.g., `dune fmt` or `dune utop`) that invoke external tools must be able to
use tools managed by `dune tools`. See [Dune Integration](#5-dune-integration).

###### 2.2.1.1 System PATH fallback

When a tool is not installed (e.g., `.ocamlformat`), dune subcommands (such as
`dune fmt`, `dune build @doc`, or `dune utop`) should fall back to the system
PATH.

**Note** This is motivated by integration with editor developers who would like
a single point of truth for running tools, and for dune to handle it. This would
mean opam users can continue to use dune in which ever way they please and the
editors will not have to care.
 


### 3. Dependency interactions

Tools installed by dune should have the minimal necessary interaction with other
dependencies of the environment they are installed in.

#### 3.1. Compiler compatibility

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

Each tool is solved independently with its own lock directory. Tool dependencies
do not affect the project's `dune.lock`, and vice versa.

See [Directory structure](./implementation.md#directory-structure) for lock
directory locations.

#### 3.3. No build triggers

Locking or adding tools must not trigger project builds.

Build rules that _use_ tools (e.g., formatting) are a separate concern; see
[Dune Integration](#5-dune-integration).

#### 3.4. Watch mode integration

Tool operations (`dune tools add`, `run`, etc.) must work correctly when a watch
server is running (`dune build -w`). Rather than directly manipulating lock
directories, tool commands should coordinate with the watch server via RPC to
avoid races and ensure the server picks up newly added tools.

CR-soon Alizter: Specify the RPC protocol for tool operations. What messages are
needed? How does the watch server respond to tool additions?

CR-soon Alizter: Document concurrent access behavior. What happens if two
terminals run `dune tools add` simultaneously? Or `add` while `run` is building?

#### 3.5. Dog fooding

Tools must work in the dune repository itself. Dune developers should be able to
run `dune tools add ocamlformat` and `dune tools add ocaml-lsp-server` when
working on dune.

This is enabled by the orthogonality design (3.2): tools are solved and built
independently from project dependencies, so they don't require a working project
lock directory.

### 4. UI

#### 4.1. CLI

Users must be able to manage tools using CLI commands:

- Add/lock individual tools to the workspace
- Run tools (building if needed)
- List locked tools and versions
- Remove tools
- Discover paths to tool executables

CLI-added tools persist until `dune clean` or `dune tools remove`, but are not
reproducible across clean builds. See [Batch
operations](./implementation.md#batch-operations) for batch commands.

#### 4.2. Persistent configuration

Users must be able to declare tools in workspace configuration. Unlike CLI-added
tools, declared tools are reproducible:

- Declaration survives `dune clean` (re-locked automatically from config)
- Declaration is version-controlled and shared with collaborators
- CI and fresh checkouts get the same tools

See [The `(tool)` stanza](./implementation.md#the-tool-stanza) for syntax.

### 5. Dune Integration

Tools must integrate with existing dune features that rely on external
executables.

#### 5.1. Format rules

`dune fmt` and `dune build @fmt` must use tools managed by `dune tools`. This
includes respecting version constraints from `.ocamlformat` files, PATH fallback
([2.4](#24-system-path-fallback)), and multi-version support
([1.5](#15-multi-version-support)).

See [Tool resolution](./implementation.md#tool-resolution) for the resolution algorithm.

#### 5.2. Documentation rules (`dune build @doc`, `dune ocaml doc`)

`dune build @doc` must use tools managed by `dune tools`. When odoc is
configured as a tool, use the locked version; otherwise fall back to PATH
([2.4](#24-system-path-fallback)).

See [Tool resolution](./implementation.md#tool-resolution) for the resolution algorithm.

CR-soon Alizter: OCaml platform considerations

#### 5.3. REPL (`dune utop`, `dune ocaml utop`)

`dune utop` must use tools managed by `dune tools`. When utop is configured as
a tool, use the locked version; otherwise fall back to PATH
([2.4](#24-system-path-fallback)).

See [Tool resolution](./implementation.md#tool-resolution) for the resolution algorithm.

#### 5.4. Tool references in actions

Build actions (both user-written rules and dune's internal rules like
formatting) must be able to reference the installed tools. E.g., via
`%{bin:...}` pforms, the base executable name, (or by some new mechanism, if
there is need for it).

#### 5.5. Legacy migration

The legacy `.dev-tools.locks/` system must be removed and replaced by this
design. Entailing updating all documentation and communicating needed users
interventions, as necessary.

<!-- CR-soon Alizter: Detail the migration story: -->

<!-- - Which CLI commands stay (with same or changed behavior)? -->
<!-- - Which CLI commands are removed? -->
<!-- - What happens to existing `.dev-tools.locks/` directories? -->
<!-- - User-facing migration guide (re-add tools via `dune tools add`) -->
