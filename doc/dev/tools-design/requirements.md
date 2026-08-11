# Dune Tools Requirements

Authors: Ali Caglayan (Tarides), Shon Feder (Tarides), Sudha Parimala (Tarides),
Ambre Suhamy (Tarides)

## Summary

This document specifies the requirements for dune's tool management system.

<!-- To regenerate TOC:
nix shell --impure --expr 'let pkgs = import (builtins.getFlake "github:NixOS/nixpkgs") {}; in (pkgs.emacs.pkgs.withPackages (ps: [ps.markdown-toc]))' -c emacs --batch --eval "(progn (require 'markdown-toc) (find-file \"doc/dev/tools-design/requirements.md\") (markdown-toc-refresh-toc) (save-buffer))"
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
        - [1.2.3. System-wide scope](#123-system-wide-scope)
      - [1.3. Version specification](#13-version-specification)
        - [1.3.1. Version consistency](#131-version-consistency)
      - [1.4. Clean source tree](#14-clean-source-tree)
      - [1.5. Binary selection](#15-binary-selection)
      - [1.6. By tool name](#16-by-tool-name)
        - [1.6.1. Disambiguation](#161-disambiguation)
      - [1.7. Project dependency tools](#17-project-dependency-tools)
      - [1.8. Discretionary tools](#18-discretionary-tools)
    - [2. Usability](#2-usability)
      - [2.1. Extent of tool management](#21-extent-of-tool-management)
        - [2.1.1. When package management is enabled in a workspace](#211-when-package-management-is-enabled-in-a-workspace)
        - [2.1.2. When package management is not enabled in a workspace](#212-when-package-management-is-not-enabled-in-a-workspace)
      - [2.2. Shells](#22-shells)
      - [2.3. Programmatic use](#23-programmatic-use)
        - [2.3.1. dune subcommands](#231-dune-subcommands)
          - [2.3.1.1. System PATH fallback](#2311-system-path-fallback)
          - [2.3.1.1.1. When package management is enabled in a workspace](#23111-when-package-management-is-enabled-in-a-workspace)
          - [2.3.1.1.2. When package management is not enabled in a workspace](#23112-when-package-management-is-not-enabled-in-a-workspace)
      - [2.4. Project dependency tools](#24-project-dependency-tools)
      - [2.5. Dog fooding](#25-dog-fooding)
    - [3. Dependency and Integration](#3-dependency-and-integration)
      - [3.1. Integration axis](#31-integration-axis)
        - [3.1.1. Tool isolation (I1)](#311-tool-isolation-i1)
          - [3.1.1.1. Optimal builds](#3111-optimal-builds)
        - [3.1.2. Tool integration (I2)](#312-tool-integration-i2)
          - [3.1.2.1. Respecting integration constraints](#3121-respecting-integration-constraints)
      - [3.2. Dependency axis](#32-dependency-axis)
        - [3.2.1. Discretionary tools (D1)](#321-discretionary-tools-d1)
          - [3.2.1.1. Not build triggers](#3211-not-build-triggers)
          - [3.2.1.2. Cannot be referenced in build rules](#3212-cannot-be-referenced-in-build-rules)
          - [3.2.1.2.1. Useful guidance on invalid reference in rules](#32121-useful-guidance-on-invalid-reference-in-rules)
          - [3.2.1.3. Subset of D2 and D3 tools functionality](#3213-subset-of-d2-and-d3-tools-functionality)
        - [3.2.2. Qualified project dependency tools (D2)](#322-qualified-project-dependency-tools-d2)
          - [3.2.2.1. Usable as tools](#3221-usable-as-tools)
          - [3.2.2.2. Installable via qualification](#3222-installable-via-qualification)
          - [3.2.2.3. Builtin D2 tools](#3223-builtin-d2-tools)
        - [3.2.3. Unqualified dependency tools (D3)](#323-unqualified-dependency-tools-d3)
          - [3.2.3.1. Usable as tools](#3231-usable-as-tools)
          - [3.2.3.2. Installed on build](#3232-installed-on-build)
    - [4. UI](#4-ui)
      - [4.1. CLI](#41-cli)
        - [4.1.1. Managing multiple tools](#411-managing-multiple-tools)
        - [4.1.2. Watch mode integration](#412-watch-mode-integration)
        - [4.1.3. Avoid invocation collisions](#413-avoid-invocation-collisions)
      - [4.2. Persistent configuration for discretionary tools](#42-persistent-configuration-for-discretionary-tools)
- [Appendix](#appendix)
  - [Tooling comparison](#tooling-comparison)
    - [Cargo](#cargo)
    - [Go](#go)
    - [uv](#uv)

<!-- markdown-toc end -->

## How to Read This Document

[Terminology](#terminology) defines key terms used throughout the document.
Precise definition of technical terms prevents misunderstandings and certain
classes of requirement error.

[Design principles](#design-principles) specifies the guiding principles
informing the design.

[Requirements](#requirements) defines _what_ capabilities the system must
provide and _why_.  Organized by category. Requirements describe user-facing
behaviour without specifying implementation details.

The [Appendix](#appendix) summarizes how cargo, go, and uv address the same
needs. Relevant points of comparison are also offered in some requirements.

## Terminology

- A **tool** is just an executable provided by some opam package. One opam
  package can provide multiple tools.
- A **project dependency** is a component (such as a library or executable)
  required for developing and/or distributing a project. For dune, this
  generally means the component must be provided by a package dependency of some
  package in the project. A component is a dependency if *any* of a project's
  build targets depend upon it, whether or not it is correctly declared as such.
- A dependency is **qualified** when it is only required for a certain category
  of build targets. Common qualifications include *test* dependencies (e.g.,
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

- **Generality**: Any executable defined in an opam package can be a tool.
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

<details>
<summary>
Motivation and context
</summary>

*Comparison with other systems*

- cargo: Follows [cargo
  install](https://doc.rust-lang.org/1.95.0/cargo/commands/cargo-install.html#description), which
  manages "Cargo’s local set of installed binary crates" and works on all (and
  only) packages that provide a binary (or executable example).
- go: Follows the [tool directive](https://go.dev/ref/mod#go-mod-file-tool),
  which can be used to make tools available from any package.
- uv: Follows the [tool
  subcommand](https://github.com/astral-sh/uv/blob/0.12.2/docs/concepts/tools.md),
  which works with any package that provides command-line interfaces.
</details>

#### 1.2. Scope

The environments within which a tool are available must be scoped.

##### 1.2.1. Workspace-local scope

It must be possible to install tools per-workspace, such that workspaces define
a sub-environment. I.e., each workspace has its own isolated tool installations
that don't affect other workspaces or interfere with software on the host system.

<details>
<summary>
Motivation and context
</summary>

**Comparison with other systems**

- *cargo* : Differs from cargo install, which installs tool globally within the
  toolchain, motivating tools such as
  [cargo-run-bin](https://github.com/dustinblackman/cargo-run-bin) that scope
  installs to within a project.
- *go* : Follows go, in which use of the [-tool
  flag](https://go.dev/doc/go1.24#tools) means installations are always scoped
  within a project's `go.mod`.
- *uv* : Differs from uv where "each tool environment is linked to a specific
  Python version",
  ([src](https://github.com/astral-sh/uv/blob/0.12.2/docs/concepts/tools.md#python-versions)).
  tools are ephemeral and non-installed by default, and an install adds them a
  `bin` dir on the users path
  ([src](https://github.com/astral-sh/uv/blob/0.12.2/docs/guides/tools.md#installing-tools)).
  Ephemerality is more obvious in a context where there is no cost beyond
  fetching sources (because no build is required). uv also supports the
  [dependency-groups][uv-dep-groups] (or the legacy
  [dev-dependencies][uv-dev-deps] an analogue of `:with-dev-setup`) scheme of
  extensible sets of qualified dependencies (see section 3.2.2).
  
</details>

[uv-dev-deps]: https://docs.astral.sh/uv/reference/settings/#dev-dependencies
[uv-dep-groups]: https://docs.astral.sh/uv/concepts/projects/dependencies/#dependency-groups

##### 1.2.2. Dune context scope

It must be possible to install the versions of a tool per-dune context within a
workspace, such that different contexts can use different versions of tools.

<details>
<summary>
Motivation and context
</summary>

This follows the example of `dune pkg`'s `lock_dir` in the context stanza, and
allows developers to set up different tooling configurations.

We have sketched a preliminary design that considers `tools` stanzas as
specialized form of `lock_dir` stanza that inherits the fields of the active
`lock_dir` in a context if no overriding fields are specified.

**Comparison with other systems**

- *cargo* : Relates to Rust's
  [toolchains](https://rust-lang.github.io/rustup/overrides.html#the-toolchain-file),
  which control the compilation context of a project and the toolchains used in
  user's environment. Only a single toolchain can be configured for a given
  project. While, Rust's [editions][rust-editions] obviate much of the need
  addressed by dune's context concept, the [current docs][rustup-docs] do signal
  a need for something akin to dune's contexts for improved cross-compilation,
  along with pending plans to address it.
- *go* : Go supports [installation of multiple Go versions][go-versions]  and
  installing other go versions just adds a new go binary suffixed with the
  alternate version, such as `go1.10.7`. Environment segregation just follows
  from use of different binaries, a route which is only available to them
  because the package manager, build system, and compiler are fully unified.
  Use of [the `-modfile` flag](https://go.dev/ref/mod#build-commands) can
  instruct a go binary to use an alternative dependency specification for
  managing a project, but this is more limited in scope than dune's contexts.
- *uv* : uv supports multiple parallel environments for a project via the
  [`UV_PROJECT_ENVIRONMENT`][uv-proj-env] environment variable. It does not have
  a single notion of contexts beyond this, but the [the uv configuration
  file][uv-config-file] can be set via `--conf-file`|`UV_CONFIG_FILE`, which
  effectively approximates dune's context management via settings like
  [sources](https://docs.astral.sh/uv/reference/settings/#sources) and
  [dev-dependencies](https://docs.astral.sh/uv/reference/settings/#dev-dependencies).

</details>

[rustup-docs]: https://github.com/rust-lang/rustup/blob/0e5a38798a7f5d6d17f46e5ac1fa184ff8031316/doc/user-guide/src/cross-compilation.md
[rust-editions]: https://doc.rust-lang.org/edition-guide/editions/
[go-versions]: https://go.dev/doc/manage-install#installing-multiple
[uv-proj-env]: https://docs.astral.sh/uv/concepts/projects/config/#project-environment-path
[uv-config-file]: https://docs.astral.sh/uv/reference/cli/#uv-auth-login--config-file

##### 1.2.3. System-wide scope

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

See the tracking issue, unpacking the motivation and presenting use cases
[dune#12107 pkg: installation of packages that can be used
system-wide](https://github.com/ocaml/dune/issues/12107).

Even if we don't support this in the first version, the design must not prevent
achieving this with a bit of user configuration.

**Comparison with other systems**

- *cargo* : Follows [`cargo install`][cargo-install], which, by default,
  satisfies this requirement by installing tools to a directory of binaries
  which is conventionally put on a user's `PATH`.
- *go* : Follows [`go install`][go-install], which, by default, satisfies this
  requirement by installing tools to a directory of binaries which is
  conventionally put on a user's `PATH`.
- *uv* : Follows the native and default behavior of [`uv tool
  install`][uv-tools-install], which installs its executables in a `bin`
  directory on the `PATH`.

</details>

[cargo-install]: https://doc.rust-lang.org/1.95.0/cargo/commands/cargo-install.html
[go-install]: https://pkg.go.dev/cmd/go#hdr-Compile_and_install_packages_and_dependencies
[uv-tools-install]: https://github.com/astral-sh/uv/blob/0.12.2/docs/guides/tools.md

#### 1.3. Version specification

Users must be able to specify the version of tools to be installed via:

- CLI arguments
- Declarative configuration (i.e. dune stanzas)
- Tool-specific configuration files (e.g., `.ocamlformat`)

<details>
<summary>
Motivation and context
</summary>

**Comparison with other systems**

- *cargo* : Via `cargo install foo@1.2.3` or via the `--version` flag. No
  support for declarative configuration currently.
- *go* : Via the version suffix of a module path.
- *uv* : [Requested][uv-versions] at the command line via `foo@1.2.3` or via
  version constraints in the `dev-dependencies` or `dependency-groups` of a
  `pyproject.toml`.
</details>

[uv-versions]: https://github.com/astral-sh/uv/blob/0.12.2/docs/guides/tools.md#requesting-specific-versions

##### 1.3.1. Version consistency

The versions of installed tools must remain consistent, accounting for all
configuration sources. E.g., consider an apparent conflict, such as a stanza
specifying `(= 0.26.2)` but a CLI input specifying `0.27.0`, or `.ocamlformat` says
`version=0.26.2` but the stanza says `(= 0.27.0)`: in such cases, a
consistent outcome must be derived. E.g., this could be achieved by having the
CLI input being used to update the config file, or simply by raising an error.
But it must not allow for a version to be installed that leads to inconsistent
version specifications.

#### 1.4. Clean source tree

Discretionary tool lock directories and built artifacts must not pollute the
source tree, to ensure that they are not inadvertently picked up in version
control or otherwise create needless noise for users. Tools that are part of
project dependencies should end up in the lock.

**Motivation**: This is a common complaint from users and is one such way to
solve the issue. Tools like `uv` handle this differently by having a global place.
Due to our compiler matching semantics it makes more sense for workspace level
and becomes fast with full caching.


<details>
<summary>
Motivation and context
</summary>

**Comparison with other systems**

- *cargo* : Satisfied by tool installations, which go to `$CARGO_HOME`.
- *go* : Satisfied by tooling executables living in a content-addressable build
  cache, outside of the source tree, though locking info goes to `go.sum`.

</details>

#### 1.5. Binary selection

When a package provides multiple tools, users must be able to specify a subset
for installation. When a package providing tools is installed without
qualification, all provided tools must be installed. As a special case, when a
package provides a single binary, it will be installed without needing to qualify.

**Motivation**: This is necessitated by the fact that the relation between opam
packages and tools is one-to-many: a single package can provide multiple
executables. As a result, it becomes necessary to only install a preferred
subset of the provided tools. E.g., `js_of_ocaml-compiler` provides
`js_of_ocaml`, `jsoo_minify`, and `jsoo_listunits`.


<details>
<summary>
Motivation and context
</summary>

**Comparison with other systems**

- *cargo* : Satisfied in cargo by the `--bin NAME` flag to install only the
  specified binary, but which installs all provided binaries by default.
- *go* : The relationship between packages and executables is 1-1 in go.
- *uv* : Satisfied in uv by the `--from` flag which allows running/installing
  "commands" (executables) "from" just the named package, e.g., `$ uvx --from
  httpie http` to run the http tool from the httpie package.

</details>

#### 1.6. By tool name

Users should be able to install tools based on the name of the tool without
considering the package that provides it.

CR Shon: because this will require changes to the opam repo to be effective,
this is a *should* rather than a *must* at the moment, and may not be achievable
in the first iteration of the redesign.

##### 1.6.1. Disambiguation

If multiple packages provide tools with the same name, and a user requests installation, dune should offer disambiguation.

CR Shon: what do we do in dune package management if two packages provide the
same executable? E.g., perhaps you want to use package a for tool a' and b for
b', but they both also provide executables named `c`?

#### 1.7. Project dependency tools

Tools that are *project dependencies* specified as appropriately qualified
dependencies in the `dune-project` file, must be installable via installation
targets reflecting the qualification, as well as through the build targets that
require them.

To illustrate, this could be through some sort of qualification to the tools
command like `dune tools install :with-test :with-dev-setup`.

<details>
<summary>
Motivation and context
</summary>

Related issues:

- [dune#12135 dune tools setup to install :with-dev-setup
  deps](https://github.com/ocaml/dune/issues/12135)
  - Should the :with-dev-setup qualifier be used by dev-tools to install?

**Comparison with other systems**

- *cargo* : Unsatisfied, see https://rust-lang.github.io/rfcs/3028-cargo-binary-dependencies.html.
- *go* : All project-specific tooling is equivalent to project-dependency
  tooling, and any discretionary tooling goes thru `go install` into the global
  binary directory.
- *uv* : Satisfied by [`dependency-groups`](https://github.com/astral-sh/uv/blob/0.12.2/docs/concepts/projects/dependencies.md#dependency-groups)

</details>

#### 1.8. Discretionary tools

It must be possible to install discretionary tools without incorrectly
specifying them as if they were project dependencies. (E.g., through a new `tool`
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

**Comparison with other systems**

- *cargo* :
  - not supported, but there is significant demand for it
  - https://users.rust-lang.org/t/request-track-dev-cli-tools-in-cargo-toml/138234
  - https://github.com/rust-lang/cargo/issues/2267
- *go* :
  - Supported via system-wide `go install`
- *uv* :
  - Supported via system-wide `uv tool install`

</details>

### 2. Usability

Users must be able to run tools installed by Dune.

#### 2.1. Extent of tool management

The extent to which dune enforces management of tools within a workspace should
depend on whether or not package management is enabled in the workspace.

##### 2.1.1. When package management is enabled in a workspace

When users enable dune package management in a workspace, all *tools* used in the
workspace (in the precise sense defined in the [terminology](#terminology))
should be managed by dune, to the extent that dune can reasonably enforce this.
Enforcing this assumption allows dune to offer users improved guarantees about
the cohesiveness and interoperability of the provided tools.

This does *not* entail that dune should mask or redact data from the `PATH` or
otherwise attempt to filter out a user's ambient environment. But it should
provide pragmatic measures to support users by enforcing this behavior where
feasible.

##### 2.1.2. When package management is not enabled in a workspace

When users have not enabled dune package management in a workspace, they must be
able to use *tools* managed by dune, but they should still be able to use tools
installed by opam (or other possible package managers) in all operations of
dune.

#### 2.2. Shells

Users must be able to run tools by invoking them directly in any shell (e.g., bash).


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

**Comparison with other systems**

- *cargo* : Satisfied by installed tools being plain executables in the `PATH`.
- *go* : Satisfied via `go install tool`, which installs tools into the
  `GOBIN`, conventionally on the `PATH`. (`go tool <name>` runs tools from the
  build cache instead, and does not put them on the `PATH`.)
- *uv* : Satisfied by installed tools being plain executables in the `PATH`.

</details>

#### 2.3. Programmatic use

Programs (e.g., editor plugins) must be able to find and run installed tools via
a single, transparent mechanism. `dune tools env` (the equivalent of `opam env`)
and `dune tools which` (which prints the path to a tool's executable) already
cover this for the currently supported set of discretionary tools; the
requirement is that the mechanism extend to every tool dune manages, including
project dependency tools (see [2.4](#24-project-dependency-tools)). The
implementation is not prescribed: it could equally be achieved by adding a
single directory of executables to the lookup path, or by some other means.


##### 2.3.1. dune subcommands

As a special case, dune subcommands (e.g., `dune fmt` or `dune utop`) that
invoke external tools must be able to use tools managed by `dune tools`, when
they are available.

###### 2.3.1.1. System PATH fallback

The extent to which dune should allow its subcommands to fallback to the system
`PATH` when looking up required binaries depends on whether or not the workspace
has enabled package management, as dictated by
[2.1](#21-extent-of-tool-management).

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

###### 2.3.1.1.2. When package management is not enabled in a workspace

When package management is not enabled in a workspace and a tool is not
installed (e.g., `.ocamlformat`), dune subcommands (such as `dune fmt`, `dune build @doc`,
or `dune utop`) should fall back to the system PATH.

**Note** This is motivated by integration with editor developers who would like
a single point of truth for running tools, and for dune to handle it. This would
mean opam users can continue to use dune in which ever way they please and the
editors will not have to care.

#### 2.4. Project dependency tools

When a tool is installed as a *project dependency* (under any qualification),
users must be able to execute the tool through all the same mechanisms that are
provided for executing discretionary tools. E.g., `dune tools env`, which makes
the path to dune-managed binaries available, must include both discretionary
tools and project dependency tools in the path.

<details>
<summary>
Motivation and context
</summary>

**Comparison with other systems**

- *cargo* : No analogue (see [RFC
  3028](https://rust-lang.github.io/rfcs/3028-cargo-binary-dependencies.html)).
- *go* : Satisfied by `go tool <name>`, which runs any tool declared with the
  `tool` directive in `go.mod`.
- *uv* : Satisfied by `uv run` executing tools installed in any group, and
  activating a project's virtual environment will bring the tools into your
  path.

</details>

#### 2.5. Dog fooding

Tools must work in the dune repository itself. Dune developers should be able to
run `dune tools install ocamlformat` and `dune tools install ocaml-lsp-server`
when working on dune.

This is enabled by the orthogonality principle (see [Design
principles](#design-principles)): discretionary tools are solved and built
independently from the project's own dependency solution, so they don't require
a working project lock directory. Tools at I2 (see [3.1.2. Tool integration
(I2)](#312-tool-integration-i2)), such as `ocaml-lsp-server`, are constrained by
the workspace's compiler version, but that constraint alone must not require the
rest of the project's dependencies to be solved.

### 3. Dependency and Integration

Tools are depended on in numerous ways to build and develop projects and they
require different levels of integration and interdependence with the other
components (tools, libraries, or other artifacts) of a workspace. Dune must be
able to manage all installable tools across this variety, otherwise the user
experience of tool management will feel inconsistent and irregular, and users
will inevitably find certain subsets of tools unavailable or requiring
unexpected workarounds creating a fragmented and awkward user experience.

We can further refine this requirement along two axes.

#### 3.1. Integration axis

Tools lie along a spectrum of integration requirements with other dependencies
in the project, which we can indicate with three points:

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
  and since the tools themselves have a wider dependency cone than the runtime
  libraries, it could sometimes be helpful to build the executables in a separate
  dependency context, pinned only to the needed runtime library version (e.g.,
  to avoid conflicts over a CLI parser library).

- I3: At the maximum extreme, some tools could require being built within the
  entire dependency context of the project. We are not aware of any tools that
  require this currently, but we can consider utop as an illustrative example,
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

Without support for dependency isolation, users would be forced to choose between
avoiding recent compiler versions or making use of these (and many other)
available tools. This would violate the design principles of orthogonality and
generality, and yield a necessarily limited usability.

</details>

###### 3.1.1.1. Optimal builds

Dune should not needlessly compile or rebuild dependencies that can be shared
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
some are used ubiquitously and because the compiler itself has a special position
in the dependency tree of any OCaml project. The most widely used tools of this
sort are ocamllsp and odoc. Dune must provide robust, intuitive, and flexible
support for managing these tools.

<details>
<summary>
Motivation and context
</summary>

One way to address this may be through an equivalent of the `constraints` field
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

</details>

###### 3.1.2.1. Respecting integration constraints

When a user has specified the intent to install an I2 tool that is incompatible
with the required packages already installed in the workspace, dune must handle
the conflict gracefully and with clear guidance to the user.

E.g., if a user tries to install the compiler-integrated `ocamllsp` and no
version is compatible with the compiler version already installed in the
workspace, dune must report and/or solve the conflict cleanly.

This could mean automatically downgrading the compiler (if permitted by
the constraints), but a user error with clear instructions is probably just as
effective, less surprising, and much easier to implement. E.g., "tool t cannot
be installed at version v because it requires compiler <= n, but compiler m is
currently installed. Add the following qualified constraint to your project
dependencies: `(ocaml (and (>= 5.1) (or (not :with-dev-setup) (< 5.5))))`" etc.

Regardless of the implementation approach taken, it must be possible to
constrain the compiler versions to work with the desired integrations during
development without those constraints polluting the constraints of packages in
the workspace when they are installed.


#### 3.2. Dependency axis

Tools lie along a spectrum of dependency status (i.e., to what extent they are
dependencies for the project) which we can indicate with these three points:

- D1. At the minimum extreme, *discretionary tools* are not project dependencies
  at all. Rather, they are tools we want to install and run on an ad hoc basis,
  for use as developers. E.g., `ocamllsp` or `ocamlgrep`.
- D2. In the midpoint, *qualified project dependency* tools are used in rules associated
  with a build alias, but not as part of the required package dependencies to
  install any packages. Example include any tools that are qualified in
  opam packages with the `with-test`, or `with-doc` filters.
- D3. At the maximum extreme, unqualified *project dependency* tools are
 required for an installation build of packages in the project, and need to be
 specified as package dependencies unconditionally. E.g., `menhir` or `atd` when
 used as part of the project build (by contrast, if these are used only for code
 generation, they could be qualified project dependencies behind a hypothetical
 `with-gen` filter).

Note that, since tools like `utop`, `odoc`, and `ocamlformat` have dedicated
builtin rules from dune, they are technically not D1.

##### 3.2.1. Discretionary tools (D1)

Dune must be able to manage discretionary tools.

###### 3.2.1.1. Not build triggers

Locking or adding discretionary tools must not trigger project builds.

###### 3.2.1.2. Cannot be referenced in build rules

Discretionary tools managed by dune must not be referenced in user build rules: dune
must not add these binaries to its path when running rules, and must not
resolve `%{bin:...}` forms that attempt to reference them. 

By definition, tools referenced in rules are at least D2.

###### 3.2.1.2.1. Useful guidance on invalid reference in rules

When a discretionary tool is configured and an invalid reference to it is found
in a build rule, dune should report an error with clear guidance to users,
advising them to move the tool configuration into the appropriate package
dependency.

###### 3.2.1.3. Subset of D2 and D3 tools functionality

The functionality of discretionary, D1 tools must be a strict subset of the
functionality of D2 and D3 project dependency tools: anything you can do with a
D1 tool, you must also be able to do with a D2 or D3 tool.

##### 3.2.2. Qualified project dependency tools (D2)

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
package specification fragmentation, since the existing system is also
supported by opam.

**Comparison with other systems**

- *cargo* : No analogue: there is no way to declare a tool that is only needed
  for a subset of build targets (see [RFC
  3028](https://rust-lang.github.io/rfcs/3028-cargo-binary-dependencies.html)).
- *go* : No analogue: the [`tool`
  directive](https://go.dev/ref/mod#go-mod-file-tool) is unqualified, so all
  declared tools are resolved in the same solve as project dependencies.
- *uv* : Satisfied by
  [`dependency-groups`][uv-dep-groups], which are extensible, named sets of
  qualified dependencies.

</details>

###### 3.2.2.1. Usable as tools

Qualified project dependency tools must be usable in at least the same ways as
discretionary, D1 tools. E.g., if D1 tools can be run via a command like `dune
tools exec ...`  then so too must D2 tools be. However, the implementation is
not prescribed by the requirement, and we could instead permit D1 tools to be
run via the existing `dune exec ...` subcommand, providing a simpler and more
intuitive interface to users than requiring two separate subcommands.

###### 3.2.2.2. Installable via qualification

If a set of tools are qualified with a filter such as `:with-test` or
`:with-doc`, it must be possible to install just that set together (in addition
to the unqualified dependencies).

###### 3.2.2.3. Builtin D2 tools

A select subset of keystone tools are treated by dune as builtin qualified
dependencies, including `odoc`, `utop`, and `ocamlformat` as the most widely
used. These should be treated as if they have qualified dependencies built in,
with further constraint or specification of how to install them available as a
user override on top of the default configuration.

##### 3.2.3. Unqualified dependency tools (D3)

###### 3.2.3.1. Usable as tools

As with [3.2.2.1](#3221-usable-as-tools), unqualified project dependency tools
must be usable in at least the same ways as D1 and D2 tools.

(Not currently satisfied by the current implementation of package management.)

###### 3.2.3.2. Installed on build

Unqualified tools must be installed when a project is built.

(Already satisfied by the current implementation of package management.)

### 4. UI

#### 4.1. CLI

Users must be able to manage tools using CLI commands:

- Add/lock individual tools to the workspace
- Run tools (building if needed)
- List locked tools and versions
- Remove tools
- Update/upgrade tools
- Discover paths to tool executables

##### 4.1.1. Managing multiple tools

It must be possible to change (e.g., update, install, or remove) a set of
configured tools by issuing a single command.

<details>
<summary>
Motivation and context
</summary>

We must not require users to do the tedious work of running the same command
over and over to install a set of tools. How this is addressed is left as an
implementation detail. E.g., it could mean supporting `dune tools install a b c`
or just having a command that installs all configured tools (see 4.2) in one
command.

Related issues:

- [dune#12557 dune tools install should take multiple package
  arguments](https://github.com/ocaml/dune/issues/12557)

</details>

##### 4.1.2. Watch mode integration

Tool operations (e.g., `dune tools install`, `exec`, etc.) must work correctly
when a watch server is running (`dune build -w`). Rather than directly
manipulating lock directories, tool commands should coordinate with the watch
server via RPC to avoid races and ensure the server picks up newly added tools.

##### 4.1.3. Avoid invocation collisions

When a `dune tools` command is run that depends on the state of the workspace,
it should not interfere with concurrent running dune commands or lead to invalid
results or states. This may just mean refusing to run if the build directory is
locked, cooperating to sequence requests, or something else.

#### 4.2. Persistent configuration for discretionary tools

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

</details>

# Appendix

## Tooling comparison

To ensure we are learning from the good work and addressed needs of other
ecosystems, we have added context to every relevant requirement indicating how
that requirement is (or is not) addressed in three other prominent package
management systems.

Please note that none of the requirements specified here are motivated by an
(otherwise unfounded) desire to achieve feature parity with these pre-existing
systems. Every requirement here voices a genuine and compelling need faced by
developers working in OCaml. Nor is our reference to implementation choices made
by the other systems intended to be prescriptive: they are only recorded to
illustrate how the need is met in those contexts.

### Cargo

cargo (Rust 1.95.0) installs tools from source with `cargo install`, into a
single user-global binary directory (`$CARGO_HOME/bin`, by default); there is no
first-party project-scoped tool management, a gap the community fills with
[cargo-run-bin](https://github.com/dustinblackman/cargo-run-bin) and
[cargo-binstall](https://github.com/cargo-bins/cargo-binstall), and that [RFC
3028](https://rust-lang.github.io/rfcs/3028-cargo-binary-dependencies.html) aims
to address for build-rule use. Keystone tools (rustfmt, clippy) ship as rustup
components version-locked to the compiler toolchain.

See https://doc.rust-lang.org/1.95.0/cargo/commands/cargo-install.html

### Go

Go (1.24+) declares tool dependencies in `go.mod` files via the `tool`
directive, resolved in the same solve as project dependencies, executed via `go
tool <name>` from the build cache, and installable to `GOBIN` with `go install
tool`. See

- https://go.dev/doc/go1.24#tools
- https://www.bytesizego.com/blog/go-124-tool-directive
- https://aran.dev/posts/go-124/go-124-new-tool-directive/

### uv

uv (Astral, v0.12.2) is a Python package and project manager whose `uv tool`
interface installs command-line tools into isolated, per-tool environments
exposed on the user’s `PATH`, and whose `uvx` alias runs tools ephemerally from
cached environments. Discretionary tools are user-scoped, not project-scoped,
and project dependency tools are supported by `dependency-groups`. See

- https://github.com/astral-sh/uv/blob/0.12.2/docs/concepts/tools.md
- https://github.com/astral-sh/uv/blob/0.12.2/docs/guides/tools.md
