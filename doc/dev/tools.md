# Tools

This document sets out the design of dune's support for managing tools (e.g.,
dev tools, such as ocamlformat or odig). The support for managing 
tools is implemented on the basis of dune package management, but we can and
should make changes to the implementation of the package management system,
when needed to achieve robust and usable tool management.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Tools](#tools)
  - [Terminology](#terminology)
  - [Requirements](#requirements)
    - [1. Installation](#1-installation)
      - [1.1. Generality](#11-generality)
      - [1.3. Project sandbox](#13-project-sandbox)
      - [1.4. System wide](#14-system-wide)
      - [1.5. Dog fooding](#15-dog-fooding)
    - [2. Usability](#2-usability)
      - [2.1. Shells](#21-shells)
      - [2.2 Programmatic use](#22-programmatic-use)
      - [2.3. Dune subcommands](#23-dune-subcommands)
      - [2.4. Dog fooding](#24-dog-fooding)
    - [3. Dependency interactions](#3-dependency-interactions)
      - [3.1. Constrained dependencies](#31-constrained-dependencies)
      - [3.2. Independence](#32-independence)
    - [4. UI](#4-ui)
      - [4.1. CLI](#41-cli)
      - [4.2. Persistent configuration](#42-persistent-configuration)
  - [TODO: Comparison with other tools](#todo-comparison-with-other-tools)
  - [TODO: Learnings from the initial dune prototype](#todo-learnings-from-the-initial-dune-prototype)

<!-- markdown-toc end -->

## Terminology

- A **tool** is just an executable provided by some opam package.
- A tool is **available** if it is part of a well formed opam package published
 on the opam repository, or via pinning from any source (e.g., a Codeberg
 repository).

## Requirements

NOTE: The requirements here should be compatible with the functional
requirements in
[https://ocaml.org/tools/platform-roadmap](https://ocaml.org/tools/platform-roadmap).
However, wherever the roadmap specifies implementation details, we are free to
deviate if needed to better satisfy the requirements.

### 1. Installation
Users must be able to install tools by Dune.

#### 1.1. Generality
All available tools must be installable, without requiring changes to the dune
source code.

#### 1.3. Project sandbox
Users must be able to install tools within the scope of a project sandbox (i.e., 
a "workspace").

#### 1.4. System wide
Users must be able to install tools in a way that allows them to be used in the
system-wide environment (e.g., outside of any particular sandbox).

*NOTE:* This does not dictate that dune must maintain the equivalent of
default switches, or predetermine any other implementation choice. But the
support for tool management must be designed in way that makes it simple and
reliable for users to use installed tools outside of a project sandbox (e.g., by
adding the location of a directory of binaries to their `PATH` or some other
means).

#### 1.5. Dog fooding
When projects include the implementation of tools, the tools must installable
from the project source.

### 2. Usability
Users must be able to run tools installed by Dune.

#### 2.1. Shells
Users must be able to run tools by invoking them directly in a shell (e.g., bash).

#### 2.2 Programmatic use
Other programs (e.g., editor plugins) must be able to find and run installed
tools via a single, transparent mechanism (e.g., an equivalent to `opam env` or
by adding a single directory of executables to the lookup path, or some other
means).

#### 2.3. Dune subcommands
When dune subcommands make use of tools (e.g., `dune fmt`), they must be able to
use the tools installed by Dune.

#### 2.4. Dog fooding
The tools installable from a project's source must be usable within the project
where they are implemented.

### 3. Dependency interactions
Tools installed by dune should have the minimal necessary interaction with
other dependencies of the environment they are installed in.

#### 3.1. Constrained dependencies
When a tool requires constraints on the version of other tools used in the same
context (e.g, ocamllsp and the ocaml compiler), this constraint must be
detected and enforced, to ensure a coherent environment.

This should be enforceable and configurable for any tools with such constraints.

#### 3.2. Independence
Any dependencies of a tool that are not constrained in the way described in
*3.1* should be installed in a separate context, to avoid unnecessary
interference with the dependencies of the rest of the context.

### 4. UI

#### 4.1. CLI

Users must be able to manage tools using the CLI interface.

#### 4.2. Persistent configuration

When users wish to share a set of installed tools, or a particular tooling
configuration, they must be able to do so via configuration data in a file
(e.g., stanzas in a `dune-workspace` etc.).

## TODO: Comparison with other tools

- uv
- cargo
- ??

## TODO: Learnings from the initial dune prototype
