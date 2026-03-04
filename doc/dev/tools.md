# Tools

Authors: Ali Caglayan (Tarides), Shon Feder (Tarides)

This document specifies dune's tool management system for development tools like
ocamlformat, ocamllsp, and odoc.

## Summary

Tools are executables you use during development but that aren't part of your
project's build output. Think of each tool as an invisible opam switch:
independently solved, with its own dependency closure, isolated from your
project.

**Why a new system?** The legacy dev-tools system only supported a hardcoded
list of tools and required dune package management. The new system works with
any opam package, whether you use dune's package management, an opam switch, or
system OCaml.

**Design principles:**

- **Generality**: Any opam package providing executables can be a tool
- **Orthogonality**: Tools are solved independently from project dependencies
- **Clean source tree**: All tool artifacts live in `_build/`, not your repo
- **Multi-version**: Different projects can use different tool versions

## How to Read This Document

**Requirements** defines _what_ capabilities the system must provide and _why_.
Organized by category (installation, usability, dependencies, UI). Requirements
describe user-facing behavior without specifying syntax or implementation.
Cross-references point to the relevant Specification sections.

**Non-requirements** explicitly lists what's out of scope and why.

**Specification** details _how_ the system implements the requirements: stanza
syntax, CLI commands, version resolution algorithm, and directory structure.
Each section notes which requirements it implements. Open questions are marked
where decisions are pending.

**Relationship to Package Management** explains the orthogonality principle and
how tools differ from project dependencies.

**Comparison with Other Tools** analyzes how uv, cargo, cargo-run-bin, and npm
handle tool management, informing our design decisions.

**CR markers** indicate areas needing further work: CR-soon for items to address
before stable release, CR-someday for post-v1 considerations.

<!-- To regenerate TOC:
nix shell --impure --expr 'let pkgs = import (builtins.getFlake "github:NixOS/nixpkgs") {}; in (pkgs.emacs.pkgs.withPackages (ps: [ps.markdown-toc]))' -c emacs --batch --eval "(progn (require 'markdown-toc) (find-file \"doc/dev/tools.md\") (markdown-toc-refresh-toc) (save-buffer))"
-->

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Tools](#tools)
  - [Summary](#summary)
  - [How to Read This Document](#how-to-read-this-document)
  - [Requirements](#requirements)
    - [Terminology](#terminology)
    - [1. Installation](#1-installation)
      - [1.1. Generality](#11-generality)
      - [1.2. Workspace-local](#12-workspace-local)
      - [1.3. Version pinning](#13-version-pinning)
      - [1.4. Multi-version support](#14-multi-version-support)
      - [1.5. Clean source tree](#15-clean-source-tree)
      - [1.6. Binary selection](#16-binary-selection)
    - [2. Usability](#2-usability)
      - [2.1. Tool invocation](#21-tool-invocation)
      - [2.2. Programmatic use](#22-programmatic-use)
      - [2.3. Dune subcommands](#23-dune-subcommands)
      - [2.4. System PATH fallback](#24-system-path-fallback)
      - [2.5. Editor integration](#25-editor-integration)
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
      - [5.1. Format rules (`dune fmt`, `dune build @fmt`)](#51-format-rules-dune-fmt-dune-build-fmt)
      - [5.2. Documentation rules (`dune build @doc`, `dune ocaml doc`)](#52-documentation-rules-dune-build-doc-dune-ocaml-doc)
      - [5.3. REPL (`dune utop`, `dune ocaml utop`)](#53-repl-dune-utop-dune-ocaml-utop)
      - [5.4. Tool references in actions](#54-tool-references-in-actions)
      - [5.5. Legacy migration](#55-legacy-migration)
    - [6. Non-requirements](#6-non-requirements)
      - [6.1. System-wide installation](#61-system-wide-installation)
      - [6.2. Automatic updates](#62-automatic-updates)
      - [6.3. Tools for tools](#63-tools-for-tools)
      - [6.4. Environment manipulation](#64-environment-manipulation)
      - [6.5. Precompiled binaries](#65-precompiled-binaries)
      - [6.6. Ephemeral runs](#66-ephemeral-runs)
  - [Specification](#specification)
    - [The `(tool)` stanza](#the-tool-stanza)
      - [Syntax](#syntax)
      - [Fields](#fields)
      - [Compiler matching](#compiler-matching)
      - [Examples](#examples)
      - [Invalid tool selections](#invalid-tool-selections)
    - [CLI commands](#cli-commands)
      - [Version syntax](#version-syntax)
      - [Batch operations](#batch-operations)
      - [Open question: version separator](#open-question-version-separator)
      - [Open question: `add` vs `install` naming](#open-question-add-vs-install-naming)
      - [Open question: `dune tools run` PATH fallback](#open-question-dune-tools-run-path-fallback)
    - [Tool resolution](#tool-resolution)
      - [Source resolution](#source-resolution)
      - [Version selection](#version-selection)
      - [Example scenarios](#example-scenarios)
    - [Directory structure](#directory-structure)
    - [Tool pforms](#tool-pforms)
      - [Syntax](#syntax-1)
      - [Resolution](#resolution)
      - [Examples](#examples-1)
      - [Future: library access](#future-library-access)
  - [Relationship to Package Management](#relationship-to-package-management)
    - [Orthogonality: a key design principle](#orthogonality-a-key-design-principle)
    - [The package-tool continuum](#the-package-tool-continuum)
    - [Key differences from project dependencies](#key-differences-from-project-dependencies)
    - [Why tools don't use `dune.lock/`](#why-tools-dont-use-dunelock)
    - [Locking vs building](#locking-vs-building)
    - [Future: Relocatable compiler and caching](#future-relocatable-compiler-and-caching)
    - [Potential future stanza options](#potential-future-stanza-options)
  - [GitHub Issues Addressed](#github-issues-addressed)
    - [Arbitrary tool packages ([#12913])](#arbitrary-tool-packages-12913)
    - [Lock directory clutter ([#10955], [#12097])](#lock-directory-clutter-10955-12097)
    - [Compiler ABI mismatch ([#11229])](#compiler-abi-mismatch-11229)
    - [Graceful fallback for missing tools ([#10578])](#graceful-fallback-for-missing-tools-10578)
    - [Version constraints ([#12777])](#version-constraints-12777)
    - [Version consistency ([#10688])](#version-consistency-10688)
    - [Dev tools redesign ([#12914])](#dev-tools-redesign-12914)
    - [Tool dependency isolation ([#12551])](#tool-dependency-isolation-12551)
    - [No project build triggers ([#11037])](#no-project-build-triggers-11037)
    - [Compiler-independent formatters ([#11038])](#compiler-independent-formatters-11038)
    - [Version from config files ([#5315])](#version-from-config-files-5315)
    - [Version constraint parsing ([#12866])](#version-constraint-parsing-12866)
    - [Opam with-dev-setup integration ([#12135])](#opam-with-dev-setup-integration-12135)
    - [Helpful error messages ([#13235], [#12975])](#helpful-error-messages-13235-12975)
    - [Batch installation ([#12557])](#batch-installation-12557)
    - [Failure isolation ([#12818])](#failure-isolation-12818)
    - [Utop findlib integration ([#13471])](#utop-findlib-integration-13471)
  - [Comparison with Other Tools](#comparison-with-other-tools)
    - [uv (Python)](#uv-python)
    - [cargo (Rust)](#cargo-rust)
    - [cargo-run-bin (Rust community tool)](#cargo-run-bin-rust-community-tool)
    - [npm (JavaScript)](#npm-javascript)
    - [How Our Design Compares](#how-our-design-compares)
      - [Tools compared](#tools-compared)
      - [Comparison table](#comparison-table)
      - [Project-local](#project-local)
      - [Multi-version](#multi-version)
      - [Declarative config](#declarative-config)
      - [Compiler matching](#compiler-matching-1)
      - [PATH fallback](#path-fallback)
      - [Precompiled binaries](#precompiled-binaries)
      - [Tool isolation](#tool-isolation)
      - [Ephemeral runs](#ephemeral-runs)
      - [Batch install](#batch-install)
      - [Binary selection](#binary-selection)
      - [Tool discovery](#tool-discovery)
      - [Tool upgrade](#tool-upgrade)
      - [System-wide install](#system-wide-install)
    - [Recommendations from Research](#recommendations-from-research)
  - [Learnings from the Prototype](#learnings-from-the-prototype)
    - [What Worked Well](#what-worked-well)
    - [Problems Discovered](#problems-discovered)
    - [Reference Packages for Testing](#reference-packages-for-testing)

<!-- markdown-toc end -->

## Requirements

NOTE: The requirements here should be compatible with the functional
requirements in
[https://ocaml.org/tools/platform-roadmap](https://ocaml.org/tools/platform-roadmap).
However, wherever the roadmap specifies implementation details, we are free to
deviate if needed to better satisfy the requirements.

### Terminology

**Tool**: An executable provided by an opam package, used during development but
not part of the project's build output. Examples: ocamlformat (formatter),
ocamllsp (language server), odoc (documentation generator), utop (REPL).

**Install**: Make a tool available for execution in the workspace. Unlike global
installation (`opam install`), tools are workspace-local. See
[CLI commands](#cli-commands) and [Directory structure](#directory-structure).

CR-soon Alizter: Ephemeral could be explained here, its a bit obscure

### 1. Installation

Users must be able to install tools via Dune.

#### 1.1. Generality

Any opam package that provides executables can be used as a tool. Unlike the
legacy system, tools are not hardcoded into dune. Users can install arbitrary
packages without waiting for dune releases.

#### 1.2. Workspace-local

Tools are installed per-workspace, not globally. Each workspace has its own
isolated tool installations that don't affect other workspaces or the system.

See [Directory structure](#directory-structure) for storage locations.

CR-soon Alizter: Once we have the relocatable compiler, tools being built will
be cached so they aren't "installed". They will materialise in \_build/ but that
shouldn't be a persistent store.

CR-someday Alizter: Consider supporting `(tool)` stanzas in `dune-project` files
for per-project tool configuration, similar to how `(pin)` exists in both
`dune-workspace` and `dune-project`. This would allow different projects within
a workspace to declare different tool requirements. Conflict resolution:
workspace wins over project, outer project wins over nested. Sibling projects
with conflicting declarations can coexist. Within project-scoped rules the
correct tool is unambiguous. CLI commands without project context (e.g.,
`dune tools run` from workspace root) should error if declarations conflict.

#### 1.3. Version pinning

Users must be able to install specific versions of tools via:

- CLI arguments
- Declarative configuration
- Tool-specific configuration files (e.g., `.ocamlformat`)

See [Version syntax](#version-syntax) for CLI syntax and
[The `(tool)` stanza](#the-tool-stanza) for declarative configuration.

CR-soon Alizter: Edge cases to specify:

- Stanza says `(= 0.26.2)` but CLI requests `0.27.0`: which wins?
- `.ocamlformat` says `version=0.26.2` but stanza says `(= 0.27.0)`: conflict?
- Version doesn't exist in opam-repository: error message?

#### 1.4. Multi-version support

Multiple versions of the same tool can coexist within a workspace. This enables
per-project tool resolution - for example, different projects within a workspace
can have different `.ocamlformat` files specifying different versions, and the
formatting rules will use the correct version for each project.

CR-someday Alizter: Multi-version scaling. Supporting N versions means N
separate solves, downloads, and builds. In a monorepo migrating ocamlformat
versions across 50 sub-projects, users could accumulate many versions. Consider:
(1) warning when version count exceeds threshold, (2) `dune tools gc` to remove
unused versions, (3) documenting expected steady-state (few versions, not many).

See [Tool pforms](#tool-pforms) for how build rules can reference versioned tool
executables.

#### 1.5. Clean source tree

Tool lock directories and built artifacts must not pollute the source tree. They
should be stored in build output directories that are:

- Excluded from version control
- Cleaned by standard build cleanup
- Invisible to users during normal development

See [Directory structure](#directory-structure) for exact paths.

CR-soon Alizter: This is a common complaint from users and is one such way to
solve the issue. Tools like uv handle this differently by having a global place.
Due to our compiler matching semantics it makes more sense for workspace level
and becomes fast with full caching.

#### 1.6. Binary selection

When a tool package provides multiple executables, users must be able to:

- Specify which binary to run (per-invocation)
- Configure a default binary (persistent)

When a package provides a single binary, it should be selected automatically.

See [The `(tool)` stanza](#the-tool-stanza) and [CLI commands](#cli-commands)
for syntax.

CR-soon Alizter: Here is an annoyance: opam packages don't specify which
binaries they install, which is fine. Once we have built the package we can
inspect which binaries we have. For something like `ocaml-lsp-server` this will
be the `ocamllsp` binary. For `ocamlformat` there are two: `ocamlformat` which
is intended and `ocamlformat-rpc` which is likely not, in this case the
prototype errors and asks the user via the CLI to be more specific by providing
the `--bin` arg. We should evaluate if this is going to be the best way to solve
this issue.

### 2. Usability

Users must be able to run tools installed by Dune.

#### 2.1. Tool invocation

Users must be able to:

- Run installed tools from any shell
- Request a specific version when multiple versions are installed

See [CLI commands](#cli-commands) for invocation syntax.

CR-soon Alizter: Edge cases to specify:

- Tool not locked: error with suggestion to run `dune tools add`?
- Multiple versions locked, none specified: error listing available versions?
- Specified version not locked: error with suggestion?
- Build fails: propagate build error?

#### 2.2. Programmatic use

Other programs (e.g., editor plugins, scripts) must be able to programmatically
discover the path to a tool's executable without parsing internal directory
structures.

See [CLI commands](#cli-commands) for the discovery interface.

CR-soon Alizter: To be clear, this is replacing the `dune tools which <tool>`
that we currently have. Not sure if its the best design.

CR-soon Alizter: `dune tools path` behavior is underspecified:

- Should `path` trigger download and build if not yet built? Or only return path
  if already built, erroring otherwise?
- If `--bin` is specified, can we compute the path without building (since path
  is deterministic from package/version/binary name)?
- If package has multiple binaries and no `--bin`: error immediately, or
  download and build first to discover available binaries?
- Tool not locked: error, or fall back to `which`?
- How does `path` interact with `(tool)` stanzas vs CLI-added tools? Same
  resolution as `run`?

CR-someday Alizter: Consider a single bin directory with symlinks to all tool
executables (like npm's `node_modules/.bin/`). Instead of per-tool paths, have
`_build/.tools/bin/` containing symlinks to all installed tool binaries. This
simplifies editor integration (one directory to add to PATH), avoids PATH length
limits (especially on Windows), and provides a single stable location for
discovery. Trade-off: need to maintain symlinks as tools are added/removed.

#### 2.3. Dune subcommands

Dune subcommands that invoke external tools must be able to use tools managed by
`dune tools`. See [Dune Integration](#5-dune-integration).

#### 2.4. System PATH fallback

When a tool is not locked and no version is specified (e.g., `.ocamlformat`
without a version), dune subcommands should fall back to the system PATH.

**Note** This is motivated by integration with editor developers who would like
a single point of truth for running tools, and for dune to handle it. This would
mean opam users can continue to use dune in which ever way they please and the
editors will not have to care.

CR-Alizter soon: Fixup wording above.

CR-Alizter soon: How would this even work if we don't know the binary from the
package?

#### 2.5. Editor integration

Editors and IDEs must be able to:

- Discover which tools are available in a workspace
- Find the path to tool executables for spawning
- Invoke tools with dune managing the process (alternative pattern)

See [CLI commands](#cli-commands) for the discovery and invocation interfaces.

**Open issues** (require consultation with editors team):

- How editors discover which tools are available
- LSP server invocation pattern: path discovery (editor spawns process) vs
  managed invocation (dune manages process). LSP servers need editor control of
  stdin/stdout for JSON-RPC and process lifecycle (restart on crash).
- Single-file formatting: `dune fmt <file>` for editor integration ([#3244]
  discusses this)

[#3244]: https://github.com/ocaml/dune/issues/3244

- `dune fmt` over RPC for watch mode integration
- Integration with `.ocamlformat` version detection
- Recommended editor configuration patterns

CR-someday Alizter: Path staleness problem. If an editor caches the result of
`dune tools path ocamllsp` and the user later updates their OCaml version, the
cached path may point to an ABI-incompatible binary. There's no mechanism to
notify editors that cached paths are invalid. Options: (1) editors always call
`dune tools path` fresh, (2) dune provides a staleness check, (3) watch mode
integration notifies editors of tool rebuilds.

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

See [Compiler matching](#compiler-matching) for the detection algorithm.

CR-someday Alizter: Reverse influence - could tools constrain the project? E.g.,
if ocamllsp isn't available for OCaml 5.3 yet, a user might want their tool
requirements to influence which compiler version they use. Currently tools are
fully isolated and don't affect project solving. This would be a significant
departure from the orthogonality principle.

#### 3.2. Dependency isolation

Each tool is solved independently with its own lock directory. Tool dependencies
do not affect the project's `dune.lock`, and vice versa.

See [Directory structure](#directory-structure) for lock directory locations.

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
working on dune, even though `dune pkg lock` doesn't work there due to the "in
and out" problem ([#8652]).

[#8652]: https://github.com/ocaml/dune/issues/8652

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
reproducible across clean builds. See [Batch operations](#batch-operations) for
batch commands.

#### 4.2. Persistent configuration

Users must be able to declare tools in workspace configuration. Unlike CLI-added
tools, declared tools are reproducible:

- Declaration survives `dune clean` (re-locked automatically from config)
- Declaration is version-controlled and shared with collaborators
- CI and fresh checkouts get the same tools

See [The `(tool)` stanza](#the-tool-stanza) for syntax.

### 5. Dune Integration

Tools must integrate with existing dune features that rely on external
executables.

#### 5.1. Format rules (`dune fmt`, `dune build @fmt`)

`dune fmt` must use tools managed by `dune tools`. This includes respecting
version constraints from `.ocamlformat` files, PATH fallback
([2.4](#24-system-path-fallback)), and multi-version support
([1.4](#14-multi-version-support)).

See [Tool resolution](#tool-resolution) for the resolution algorithm.

#### 5.2. Documentation rules (`dune build @doc`, `dune ocaml doc`)

`dune build @doc` must use tools managed by `dune tools`. When odoc is
configured as a tool, use the locked version; otherwise fall back to PATH
([2.4](#24-system-path-fallback)).

See [Tool resolution](#tool-resolution) for the resolution algorithm.

CR-soon Alizter: OCaml platform considerations

#### 5.3. REPL (`dune utop`, `dune ocaml utop`)

`dune utop` must use tools managed by `dune tools`. When utop is configured as
a tool, use the locked version; otherwise fall back to PATH
([2.4](#24-system-path-fallback)).

See [Tool resolution](#tool-resolution) for the resolution algorithm.

#### 5.4. Tool references in actions

Build actions (both user-written rules and dune's internal rules like
formatting) must be able to reference tool executables via pforms. If the tool
is not locked, fall back to PATH (consistent with
[2.4](#24-system-path-fallback)).

See [Tool pforms](#tool-pforms) in the Specification for syntax.

#### 5.5. Legacy migration

The legacy `.dev-tools.locks/` system is removed and replaced by this design.

CR-soon Alizter: Detail the migration story:

- Which CLI commands stay (with same or changed behavior)?
- Which CLI commands are removed?
- What happens to existing `.dev-tools.locks/` directories?
- User-facing migration guide (re-add tools via `dune tools add`)

### 6. Non-requirements

The following are explicitly out of scope:

#### 6.1. System-wide installation

Tools are workspace-local, not installed system-wide like `opam install` or
`cargo install`. There is no global tool registry outside of workspaces.

Note: While tools are workspace-local, builds are still cached via the dune
build cache. If two workspaces install the same tool version with the same
compiler, the second workspace reuses the cached build artifacts.

CR-someday Alizter: Consider system-wide tool installation for use outside
workspaces. Tools could be stored in `~/.cache/dune/tools/` and invoked via
`dune tools run` from any directory. This would enable:

- Global utop without an opam switch
- Running OCaml scripts with dependencies (`dune run script.ml`)
- `dune shell` outside workspaces The same isolation and compiler-matching logic
  applies. Related: [#12107] (system-wide packages).

[#12107]: https://github.com/ocaml/dune/issues/12107

#### 6.2. Automatic updates

Tools do not auto-update; explicit user action is required.

#### 6.3. Tools for tools

Using tools while developing the tools themselves (or their dependencies) has
nuances related to the "in and out" problem ([#8652]).

**Different scenarios**:

- **Working on a tool's dependency** (e.g., a library that ocamlformat uses):
  You probably don't need ocamlformat to use your local changes - you just want
  ocamlformat to work for formatting your code. The standard tool isolation
  handles this fine.

- **Working on the tool itself** (e.g., developing ocamllsp): You likely want to
  use the ocamllsp you're building, not a separately installed version. This is
  where pins could help - pin the tool to the local path. But circular builds
  (tool needs to build itself) remain unsolved.

The first scenario is common and supported. The second is out of scope until the
in-and-out problem is resolved, though pins may provide a partial workaround for
non-circular cases.

#### 6.4. Environment manipulation

No `dune tools env` command. Unlike `opam env`, we avoid stateful shell
environment manipulation. Use `dune tools run` or `dune tools path` instead.

We acknowledge this adds friction for users migrating from opam and for
interactive shell use. However, we believe explicit invocation is preferable to
stateful environment that can silently become stale or inconsistent.

CR-soon Alizter: There's a tension here: we reject `dune tools env` as too
implicit, but PATH fallback (2.4) is also implicit. Clarify the philosophy: is
the issue stateful shells specifically, or implicit behavior generally?

CR-soon Alizter: dune tools env exists in the prototype, but it may be
undesirable.

CR-someday Alizter: Consider `dune shell` as an alternative to `dune tools env`.
Instead of mutating the current shell's environment (opam-style), spawn a
subshell with tools in PATH (Nix-style). This avoids stale environment issues
and makes the boundary clear - just exit the subshell to return to normal. Could
also support `dune shell --with ocamlformat ocamllsp` syntax.

#### 6.5. Precompiled binaries

Tools are always built from source. Downloading precompiled binaries (like
cargo-binstall or uv's wheel downloads) is out of scope.

This ensures compiler compatibility (Req 3.1) and avoids ABI (Application Binary
Interface) mismatch issues. OCaml binaries embed compiler-version-specific
formats - for example, `.cmi` files have checksums tied to compiler internals. A
pre-built tool (e.g., ocamllsp compiled with a different toolchain) cannot read
build artifacts from a differently-compiled project, causing "corrupted compiled
interface" errors ([#11229]).

[#11229]: https://github.com/ocaml/dune/issues/11229

#### 6.6. Ephemeral runs

No `uvx`-style temporary execution mode. Tools must be locked before running.

Building OCaml tools from source is slow compared to downloading Python wheels,
so discarding the build after one use would be wasteful. Locking caches the
build for reuse. Users who want to try a tool temporarily can use
`dune tools add`, run it, then `dune tools remove`.

CR-soon Alizter: Mention that autolocking makes this somewhat feasable, but it cannot
be relied on as it will roll to the newer versions. There are no binary
installs so therefore it would be slow also

## Specification

### The `(tool)` stanza

_Implements: [1.3 Version pinning](#13-version-pinning),
[1.6 Binary selection](#16-binary-selection),
[3.1 Compiler compatibility](#31-compiler-compatibility),
[4.2 Persistent configuration](#42-persistent-configuration)_

Tools are declared in `dune-workspace` using the `(tool)` stanza. The stanza
uses the same dependency syntax as `(depends ...)` for the package field.

#### Syntax

```lisp
(tool
 (package <package-dependency>)   ; Required: package name with optional constraint
 (executable <string>)            ; Optional: binary name, defaults to package name
 (skip_compiler_match)            ; Optional: flag to disable compiler matching
 (repositories <repo-name> ...))  ; Optional: restrict to specific repositories
```

CR-soon Alizter: missing pin

CR-soon Alizter: Should share some of lock_dir stanza fields. Need to do a
comparison and explain why it is or is not a good idea.

CR-soon Alizter: Edge cases for stanza parsing:

- Duplicate stanzas for same package with different constraints: error, or
  merge?
- Invalid package name syntax: error message?
- Stanza references non-existent repository: when to error (parse time vs solve
  time)?

#### Fields

- **`(package <dep>)`** (required): The opam package providing the tool. Accepts
  either a plain package name or a name with version constraint:
  - Plain: `ocamlformat`
  - With constraint: `(ocamlformat (= 0.26.2))`
  - Version operators: `=`, `<>`, `<`, `>`, `<=`, `>=`
  - Conjunctions: `(and (>= 0.25.0) (< 0.27.0))`

- **`(executable <name>)`** (optional): The binary to run. Defaults to the
  package name. Required when the package provides multiple binaries.

- **`(skip_compiler_match)`** (optional): When present, disables the default
  compiler matching behavior. Use this for tools that don't need compiler
  compatibility (e.g., formatters that only parse source text).

CR-soon Alizter: The prototype uses `compiler_compatible` (opt-in) but this doc
says `skip_compiler_match` (opt-out). Reconcile naming and semantics.

- **`(repositories <names>)`** (optional): Restrict package resolution to the
  named repositories. Useful for binary package repositories.

CR-soon Alizter: The `(repositories)` field is parsed but verify that the
restriction is actually applied during solving in the prototype.

CR-soon Alizter: Missing pins

#### Compiler matching

_Implements: [3.1 Compiler compatibility](#31-compiler-compatibility)_

Tools that read project build artifacts (e.g., ocamllsp reading `.cmi` files)
must be built with a compatible compiler to avoid ABI mismatches. By default,
tools are built with a compiler matching the environment.

The compiler is detected in this order:

1. **Project compiler** - if using dune package management with a `dune.lock/`
2. **System OCaml** - from opam switch or PATH
3. **Any compatible version** - if no compiler is detected

When `(skip_compiler_match)` is set, the tool is solved without compiler
constraints, allowing it to use any OCaml version.

CR-soon Alizter: Edge cases for compiler matching:

- No compiler found (no opam switch, no system OCaml): error, or pick any?
- System OCaml version changes between `add` and `run`: rebuild, or warn?
- Tool requires newer OCaml than project: error message?

#### Examples

```lisp
;; Minimal: just the package name
(tool
 (package ocamlformat))

;; With version constraint
(tool
 (package
  (ocamlformat
   (= 0.26.2))))

;; With version range
(tool
 (package
  (ocamlformat
   (and
    (>= 0.25.0)
    (< 0.27.0)))))

;; Specifying the binary for multi-binary packages
(tool
 (package menhir)
 (executable menhirSdk))

;; Tool that doesn't need compiler matching
(tool
 (package ocamlformat)
 (skip_compiler_match))

;; Using a binary package repository
(tool
 (package ocaml-lsp-server)
 (repositories binary-packages))
```

#### Invalid tool selections

Some packages cannot be used as tools:

- **Library-only packages**: Packages that provide no executables (e.g., `base`,
  `lwt`) cannot be tools. Error at `dune tools add` time.

CR-someday Alizter: The `%{tool:lib:...}` pform in [Tool pforms](#tool-pforms)
could enable library-only tool packages, but this needs further design work.

CR-soon Alizter: How can we error at dune tools add time? We would only find out
after building the tool.

- **Compiler packages**: Selecting `ocaml-base-compiler` or `ocaml-system` as a
  tool is invalid. These conflict with the compiler matching logic.

- **Dune itself**: Using `dune` as a tool is the "in and out" problem
  ([non-requirement 6.3](#63-tools-for-tools)). Circular dependency issues make
  this out of scope.

CR-soon Alizter: The explanations for compiler packages and dune itself need
more thought.

CR-soon Alizter: Specify exact error messages for these cases.

### CLI commands

_Implements: [1.3 Version pinning](#13-version-pinning),
[1.6 Binary selection](#16-binary-selection),
[2.1 Tool invocation](#21-tool-invocation),
[2.2 Programmatic use](#22-programmatic-use), [4.1 CLI](#41-cli)_

CR-soon Alizter: The CLI commands are generally underspecified. Each command
needs detailed behavior documentation: what triggers downloads/builds, how
errors are reported, interaction with stanzas vs CLI-added tools, exit codes,
output format, etc.

```
dune tools                                         List all locked tools (alias for list)
dune tools add                                     Lock all tools from (tool) stanzas
dune tools add <pkg>[.<version>] ...               Lock specific tool(s)
dune tools run <pkg>[.<version>] [--bin <name>] [-- <args>]
                                                   Run a tool, passing arguments after --
dune tools path <pkg>[.<version>] [--bin <name>]   Print path to tool executable
dune tools list                                    List all locked tools and versions
dune tools remove <pkg>[.<version>] ...            Remove a tool's lock directory
```

The `--bin` flag is required when a package provides multiple binaries and no
`(executable ...)` is specified in the `(tool)` stanza.

CR-soon Alizter: Edge cases for `dune tools add`:

- Package doesn't exist in repository: error message?
- Network unavailable: error message, offline mode?
- Solver fails (unsatisfiable constraints): error with diagnostics?
- Build fails: partial state cleanup?
- Interrupted mid-install: cleanup, or resume on next run?
- Already locked at same version: no-op, or re-solve?
- Already locked at different version: add second version, or replace?

CR-soon Alizter: The prototype has `--allow-not-installed` flag for
`dune tools path`. Document this flag and its use case (editor integration).

#### Version syntax

The CLI uses dot-separated version syntax: `<package>.<version>`

```bash
dune tools add ocamlformat           # Latest version
dune tools add ocamlformat.0.26.2    # Specific version
```

#### Batch operations

Batch operations apply to commands that manage tool state, not to commands that
use tools:

| Command             | Batch | Rationale                                       |
| ------------------- | ----- | ----------------------------------------------- |
| `dune tools add`    | ✓     | No args: all from stanzas; with args: specified |
| `dune tools remove` | ✓     | Multiple packages                               |
| `dune tools run`    | ✗     | Running is inherently single-target             |
| `dune tools path`   | ✗     | Path lookup is per-tool                         |

CR-soon Alizter: Edge cases for `dune tools remove`:

- Tool not locked: error, or no-op with warning?
- Tool declared in stanza: remove lock but warn it will be re-added?
- Multiple versions locked: remove all, or require version specifier?
- Remove while tool is running: behavior?

**`dune tools add`** has two modes:

```bash
dune tools add                                # Lock all from (tool) stanzas
dune tools add ocamlformat odoc               # Lock specific packages
dune tools add ocamlformat.0.26.2 odoc.2.4.0  # With version constraints
```

The no-argument form is critical for CI and onboarding - a single command to set
up all project tooling from declarative configuration.

CR-someday Alizter: Integration with opam's `:with-dev-setup` marker ([#12135]):

[#12135]: https://github.com/ocaml/dune/issues/12135

Projects often declare their development tools in opam files using the
`:with-dev-setup` filter (e.g., `ocamlformat :with-dev-setup`). Currently, dune
tools doesn't read these - users must manually add each tool or duplicate the
declarations in `(tool)` stanzas.

One possible approach:

```bash
dune tools add --with-dev-setup   # Install tools from opam :with-dev-setup deps
dune tools add --with-doc         # Install tools from :with-doc deps
```

This would enable:

- Single source of truth for project tooling (opam file)
- Automatic tool discovery for new contributors
- CI scripts that don't need to know which tools to install

CR-soon Alizter: Is `--with-dev-setup` the right approach? Alternatives:

- Automatic: `dune tools add` (no args) always includes `:with-dev-setup` deps
- Separate command: `dune tools setup` specifically for opam marker integration
- No integration: keep `(tool)` stanzas as the only source, avoid duplication by
  not using `:with-dev-setup` in opam files at all The flag approach adds
  complexity. Need to evaluate if the use case justifies it.

CR-soon Alizter: If we do integrate, design questions:

- Read from `dune-project` `(depends)` or from `.opam` files?
- What if opam file says `ocamlformat` but `.ocamlformat` says `version=0.26.2`?
- How does this interact with `(tool)` stanzas that specify the same package?

CR-soon Alizter: The prototype requires at least one package argument. The
no-argument batch mode (lock all from stanzas) is not yet implemented.

#### Open question: version separator

The current prototype uses `.` as the version separator (`pkg.version`), aligned
with opam conventions. However, `@` is common in other ecosystems:

- **npm/npx**: `npx cowsay@1.5.0`
- **cargo**: `cargo install ripgrep@14.0.0`

Opam package names cannot contain dots (allowed: `[a-zA-Z0-9_+-]+`), so `.` is
unambiguous. The choice is primarily about familiarity vs ecosystem consistency.

#### Open question: `add` vs `install` naming

The current prototype uses `dune tools add` for adding individual tools.
However, the naming convention is still open for discussion:

- **Option A**: `add` for individual, `install` for batch (current prototype)
- **Option B**: `install` for both (consistent with legacy, npm, cargo)
- **Option C**: `lock` to emphasize the solving/locking aspect
- **Option D**: Semantic continuum where commands differ by how much work they
  do:
  - `add` = lock only (solve + write lock)
  - `install` = lock + build (download + compile)
  - `run` = lock + build + execute This gives users control over when work
    happens (useful for CI, Docker builds).

Considerations:

- Legacy issues reference `dune tools install`
- `add` suggests incremental, `install` suggests setup
- `lock` aligns with `dune pkg lock` but may confuse (tools also build)
- Option D aligns with user expectations: "add to config" vs "install software"

#### Open question: `dune tools run` PATH fallback

Should `dune tools run <pkg>` fall back to system PATH when the tool is not
locked?

- **Option A**: Fall back to PATH (consistent with build rules like `dune fmt`)
- **Option B**: Error with suggestion to lock or install on PATH (current impl)

Considerations:

- Falling back makes `dune tools run` a universal "run this tool" command
- Erroring is more explicit and avoids silently using unexpected versions
- Build rules need PATH fallback for gradual adoption (e.g., `dune fmt` without
  locking ocamlformat)
- CLI commands could be stricter since the user explicitly requested a tool

### Tool resolution

_Implements: [1.4 Multi-version support](#14-multi-version-support),
[2.4 System PATH fallback](#24-system-path-fallback),
[5.1 Format rules](#51-format-rules-dune-fmt)_

Tool resolution answers two questions:

1. **Source**: Where does the tool come from?
2. **Version**: Which version of the tool?

#### Source resolution

When dune needs a tool, it checks these sources in order:

1. **Workspace `(tool)` stanza** - declared in `dune-workspace`
2. **Locked versions** - previously added via `dune tools add`
3. **System PATH** - fallback if nothing else found

The first source that has the tool wins. If no source has it, dune falls back to
system PATH (which may result in "command not found").

#### Version selection

CR-soon Alizter: Version conflict resolution algorithm is undefined. What
happens when multiple sources specify conflicting versions? E.g., stanza says
`(= 0.26.2)` but `.ocamlformat` says `version=0.27.0`. Need to define precedence
or error behavior.

How the version is determined depends on context:

**CLI commands** (`dune tools run`, `dune tools add`):

- Explicit version: `dune tools run ocamlformat.0.26.2` uses that version
- No version: uses the single locked version, or errors if multiple exist

**`(tool)` stanza**:

- Version constraint in stanza: `(package (ocamlformat (= 0.26.2)))`
- Constraint applied when locking, not at runtime

**Formatting rules** (`dune fmt`):

1. Read `version` field from `.ocamlformat` file
2. If version present: use `%{tool:ocamlformat.<version>}` pform
3. If no version: fall back to `%{bin:ocamlformat}` (system PATH)

The pform handles locking/building/PATH fallback per the resolution rules above.

#### Example scenarios

| Scenario                                    | Source         | Version        |
| ------------------------------------------- | -------------- | -------------- |
| `(tool (package ocamlformat))` in workspace | stanza         | locked version |
| `dune tools add ocamlformat.0.26.2`         | locked         | 0.26.2         |
| `.ocamlformat` says `version=0.26.2`        | locked or PATH | 0.26.2         |
| No config, ocamlformat on PATH              | PATH           | system version |

CR-soon Alizter: Specify error messages for failure modes: solver fails, network
error, build fails, binary not found, multiple binaries without --bin, etc. Is
the locked version the most recent?

### Directory structure

_Implements: [1.2 Workspace-local](#12-workspace-local),
[1.5 Clean source tree](#15-clean-source-tree),
[3.2 Dependency isolation](#32-dependency-isolation)_

Tools use three types of directories:

1. **External lock directories** - Created by `dune tools add`, contain solver
   output
2. **Build lock directories** - Internal copies for build rules
3. **Install directories** - Built artifacts (binaries, libraries)

```
_build/
├── .tools.lock/                      # External lock directories (solver output)
│   └── <package>/
│       └── <version>/
│           ├── lock.dune             # Lock file
│           └── <package>.pkg         # Package metadata
└── _private/
    └── default/
        ├── .tool-locks/              # Build lock directories (internal)
        │   └── <package>/
        │       └── <version>/
        │           ├── lock.dune
        │           └── <package>.pkg
        └── .tools/                   # Install directories (built artifacts)
            └── <package>/
                └── <version>/
                    └── target/
                        ├── bin/<exe> # Installed executables
                        └── cookie    # Installation trace (artifacts + variables)
```

**Path flow**: External lock → copied to build lock → built to install dir.

**Key paths**:

- External lock: `_build/.tools.lock/<pkg>/<version>/`
- Build lock: `_build/_private/default/.tool-locks/<pkg>/<version>/`
- Install: `_build/_private/default/.tools/<pkg>/<version>/target/`

Note: The external lock directory (`_build/.tools.lock/`) is at the build root,
not inside `_private/`. This allows tools to be locked without going through the
build system. The build lock directory and install directory are both inside
`_private/default/` as they are managed by the build system.

### Tool pforms

_Implements: [5.4 Tool references in actions](#54-tool-references-in-actions)_

Build actions can reference tool executables via `%{tool:...}` pforms.

#### Syntax

```
%{tool:<pkg>}              ; executable (default binary)
%{tool:<pkg>.<version>}    ; specific version
```

#### Resolution

1. If tool is locked, use the locked executable path
2. If tool is not locked, fall back to PATH (like `%{bin:...}`)

CR-soon Alizter: What happens when version is omitted and multiple versions are
locked? Error, or pick one?

#### Examples

```lisp
;; Run a tool executable
(rule
 (action (run %{tool:ocamlformat} %{deps})))

;; Use a specific version
(rule
 (action (run %{tool:ocamlformat.0.26.2} %{deps})))
```

#### Future: library access

A future extension could allow accessing libraries from tool packages via
`%{tool:lib:<pkg>}`. This would enable using a library solved independently from
the project's dependency graph.

However, this has significant drawbacks: if two tool libraries share a common
dependency (e.g., both depend on `base`), they are solved independently and may
have incompatible versions. Using both in the same build rule could cause
linking errors. This needs further design work around "tool groups" or coherence
constraints before being viable.

## Relationship to Package Management

Tools are built on the same infrastructure as `dune pkg` (Dune's package
management):

- Same solver (`opam-0install-solver`)
- Same repository system (opam-repository, overlays, custom repos)
- Same lock file format
- Same build rules for fetching and compiling packages

### Orthogonality: a key design principle

Tools are **orthogonal** to project package management. This decoupling
simplifies the design:

- **No project lock required**: Tools work even without `dune.lock/`. A project
  can use `dune tools` without enabling package management.
- **Independent solving**: Adding/removing tools doesn't affect project
  dependencies, and vice versa.
- **No build invalidation**: Tool changes don't trigger project rebuilds
  ([requirement 3.3](#33-no-build-triggers)).
- **Multi-version is natural**: Since each tool version is solved independently,
  supporting multiple versions falls out of the design.

The legacy system violated this principle by mixing tool dependencies with
project packages (the "wrapper package hack"), causing solver conflicts and
complex regeneration logic.

### The package-tool continuum

Rather than a binary choice between "package" and "tool", the design supports a
continuum of integration levels:

**Fully integrated (packages)**: Dependencies in `dune.lock/` are solved
together, libraries and executables are available throughout the build, and
changes affect the entire project.

**Fully isolated (tools)**: Each tool has its own lock directory, is solved
independently, and by default contributes nothing to the build environment. The
tool sits in `_build/` invisible to the project unless explicitly invoked.

**Selective exposure (via pforms)**: The `%{tool:...}` pform
([Tool pforms](#tool-pforms)) allows build rules to selectively reference tool
executables. This lets a tool remain isolated (independently solved, no
dependency conflicts) while still being usable in specific rules.

This continuum lets users choose the right level of integration: use packages
when you need tight coupling and shared dependencies; use tools when you want
isolation and version flexibility; use pforms when you need something in
between.

### Key differences from project dependencies

| Aspect        | Project packages (`dune pkg`)     | Tools (`dune tools`)               |
| ------------- | --------------------------------- | ---------------------------------- |
| Lock location | `dune.lock/` (committed)          | `_build/.tools.lock/` (ephemeral)  |
| Solving       | All packages solved together      | Each tool solved independently     |
| User workflow | `dune pkg lock` then `dune build` | `dune tools add`, builds on demand |
| Compiler      | Project's compiler                | Matches project or system OCaml    |
| Requires lock | Yes                               | No (can use system OCaml)          |

CR-soon Alizter: system-ocaml, the opam package, is a bit of a hack. If a user
changes their opam switch it might not work as intended.

CR-soon what does the Requires lock even mean here?

### Why tools don't use `dune.lock/`

Tools are stored in `_build/` rather than alongside `dune.lock/` because:

1. **Tools are workspace infrastructure**, not project deliverables
2. **Different sub-projects may need different tool versions** (e.g., different
   `.ocamlformat` versions per directory)
3. **Avoids cluttering source tree** (addresses [#10955], [#12097])

[#10955]: https://github.com/ocaml/dune/issues/10955
[#12097]: https://github.com/ocaml/dune/issues/12097

The tradeoff is that tool lock directories are not committed to version control.
Reproducibility comes from `(tool)` stanzas in `dune-workspace`, which declare
version constraints that are re-solved when `_build/` is deleted.

### Locking vs building

These are separate phases, though `dune tools add` combines them:

1. **Locking**: Solve dependencies, write lock directory with package metadata
2. **Building**: Download sources, compile tool, install to target directory

Building (which includes downloading sources) happens lazily on first
`dune tools run`, not during `dune tools add`. The `add` command only solves
dependencies and writes the lock directory.

**Open question**: How can users download, build, and install tools without
running them? This is useful for CI (prepare all tools upfront) or offline
scenarios. Options include a `--build` flag on `add`, a separate
`dune tools build` command, or relying on `dune tools path` to trigger builds.

### Future: Relocatable compiler and caching

Currently, each workspace builds its own copy of tools, even if the same tool
version is used across multiple projects. This is wasteful.

The **relocatable compiler** work will enable:

- **Shared build cache**: Tool builds can be cached and reused across projects
- **Faster tool installation**: If another project already built
  `ocamlformat.0.26.2` with the same compiler, reuse the cached artifacts
- **Binary distribution**: Pre-built tool binaries could be distributed via
  binary package repositories

This will address the performance concerns of
[non-requirement 6.1](#61-system-wide-installation) (System-wide installation)
while maintaining project isolation.

### Potential future stanza options

The `(lock_dir)` stanza for project packages supports options not yet available
for tools:

- `(solver_env ...)` - inject variables into solver
- `(constraints ...)` - additional solver constraints
- `(version_preference newest|oldest)` - prefer newest or oldest versions
- `(pins ...)` - pin packages to specific sources

These may be added to `(tool)` stanzas if use cases emerge. In particular,
`(pins ...)` would help with [non-requirement 6.3](#63-tools-for-tools) (Tools
for tools) by allowing tools to be pinned to local paths.

## GitHub Issues Addressed

This section maps known issues to the requirements that address them.

### Arbitrary tool packages ([#12913])

**Problem**: The current design requires every tool to be hardcoded. Only a few
tools are supported, and adding new ones requires code changes to dune.

**Addressed by**: Requirement 1.1 (Generality). Any opam package that provides
an executable can be installed as a tool via `dune tools add <pkg>`.

[#12913]: https://github.com/ocaml/dune/issues/12913

### Lock directory clutter ([#10955], [#12097])

**Problem**: Users complained about lock directories cluttering the source tree.

**Addressed by**: Requirement 1.5 (Clean source tree). Tool lock directories are
now stored in `_build/.tools.lock/`, not the source tree.

[#10955]: https://github.com/ocaml/dune/issues/10955
[#12097]: https://github.com/ocaml/dune/issues/12097

### Compiler ABI mismatch ([#11229])

**Problem**: Pre-built tool binaries (musl toolchain) don't work with
locally-built OCaml compilers because interface hashes don't match.

**Addressed by**: Requirement 3.1 (Compiler compatibility). Tools are built from
source with matching compiler constraints by default.

[#11229]: https://github.com/ocaml/dune/issues/11229

### Graceful fallback for missing tools ([#10578])

**Problem**: `dune fmt` fails hard when ocamlformat isn't installed, even if
other formatters could run.

**Addressed by**: Requirements 2.4 (System PATH fallback) and 5.1 (Format
rules). When no version is specified in `.ocamlformat`, dune falls back to PATH.
If not found, the OCaml formatting rules are skipped rather than failing the
entire build.

[#10578]: https://github.com/ocaml/dune/issues/10578

### Version constraints ([#12777])

**Problem**: No robust way to specify version constraints for dev tools. Users
had to hardcode paths to lock directories.

**Addressed by**: Requirements 1.3 (Version pinning) and 4.2 (Persistent
configuration). The `(tool)` stanza allows declarative version constraints:

```lisp
(tool
 (package
  (ocamlformat
   (= 0.26.2))))
```

[#12777]: https://github.com/ocaml/dune/issues/12777

### Version consistency ([#10688])

**Problem**: When ocamlformat isn't a project dependency, `dune fmt` captures
whatever version is on PATH, leading to inconsistent formatting.

**Addressed by**: Requirements 1.3 (Version pinning) and 5.1 (Format rules).
When `.ocamlformat` specifies a version, dune uses the locked version. The PATH
is only used as fallback when no version is specified.

[#10688]: https://github.com/ocaml/dune/issues/10688

### Dev tools redesign ([#12914])

**Problem**: Meta-issue tracking the need for a complete redesign of dev tools.

**Addressed by**: This entire design document. Key improvements:

- Generality (1.1): Any package, not just hardcoded ones
- Clean source tree (1.5): Lock dirs in `_build/`
- Version pinning (1.3): Declarative constraints via stanzas
- Multi-version (1.4): Different projects can use different versions
- Compiler compatibility (3.1): Tools built with matching compiler

[#12914]: https://github.com/ocaml/dune/issues/12914

### Tool dependency isolation ([#12551])

**Problem**: Tools and project packages conflict when they share dependencies.

**Addressed by**: Requirements 3.2 (Dependency isolation) and 5.3 (REPL). Each
tool has its own lock directory and is solved independently from project
dependencies.

[#12551]: https://github.com/ocaml/dune/issues/12551

### No project build triggers ([#11037])

**Problem**: Running `dune fmt` triggers a full build of project dependencies,
even though formatting only needs ocamlformat.

**Addressed by**: Requirements 3.3 (No build triggers) and 5.1 (Format rules).
Tool operations are isolated from project builds.

[#11037]: https://github.com/ocaml/dune/issues/11037

### Compiler-independent formatters ([#11038])

**Problem**: `dune fmt` fails if no OCaml compiler is available, even though
ocamlformat could be a standalone binary.

**Addressed by**: Requirement 3.1 (Compiler compatibility). Tools match the
project or system compiler, but formatters can opt out via
`(skip_compiler_match)`.

[#11038]: https://github.com/ocaml/dune/issues/11038

### Version from config files ([#5315])

**Problem**: The `version` field in `.ocamlformat` should determine which
ocamlformat version to use. Different projects within a workspace may have
different `.ocamlformat` files specifying different versions.

**Addressed by**: Requirements 1.3 (Version pinning), 1.4 (Multi-version
support), and 5.1 (Format rules). Formatting rules resolve the correct
ocamlformat version based on each directory's `.ocamlformat` file, and multiple
versions can coexist in the workspace.

[#5315]: https://github.com/ocaml/dune/issues/5315

### Version constraint parsing ([#12866])

**Problem**: Version constraints for dev tools don't work correctly.

**Addressed by**: Requirement 1.3 (Version pinning). The new `(tool)` stanza
properly supports version constraints using dependency syntax.

[#12866]: https://github.com/ocaml/dune/issues/12866

### Opam with-dev-setup integration ([#12135])

**Problem**: Projects declare development tools in opam files using the
`:with-dev-setup` filter, but dune tools doesn't integrate with this. Users must
manually discover and add each tool, or duplicate declarations in both opam
files and `(tool)` stanzas.

**Addressed by**: Requirement 4.1 (CLI), future work. See "Batch operations" in
the Specification for the planned `--with-dev-setup` flag.

[#12135]: https://github.com/ocaml/dune/issues/12135

### Helpful error messages ([#13235], [#12975])

**Problem**: When tools aren't installed, error messages don't guide users
toward the correct commands.

**Addressed by**: Requirement 4.1 (CLI). Commands should provide helpful
suggestions when tools aren't installed.

[#13235]: https://github.com/ocaml/dune/issues/13235
[#12975]: https://github.com/ocaml/dune/issues/12975

### Batch installation ([#12557])

**Problem**: Installing multiple tools requires multiple commands.

**Addressed by**: Requirement 4.1 (CLI). `dune tools add` accepts multiple
package arguments, and with no arguments locks all tools from `(tool)` stanzas.
See "Batch operations" in the Specification.

[#12557]: https://github.com/ocaml/dune/issues/12557

### Failure isolation ([#12818])

**Problem**: Failed tool installation can leave the environment in a broken
state.

**Addressed by**: Requirement 3.2 (Dependency isolation). Each tool is isolated,
so a failed install doesn't affect other tools or the project.

[#12818]: https://github.com/ocaml/dune/issues/12818

### Utop findlib integration ([#13471])

**Problem**: Running `#require` in utop triggers findlib warnings about
non-existent directories from dune's sandboxed package paths.

**Addressed by**: Requirement 5.3 (REPL). When utop is configured as a tool, it
should be properly integrated with dune's package environment.

[#13471]: https://github.com/ocaml/dune/issues/13471

## Comparison with Other Tools

This section documents how other ecosystems handle tool management, informing
our design decisions and identifying gaps in our approach.

### uv (Python)

[uv][uv docs] distinguishes between ephemeral and persistent tool usage:

- **`uvx <tool>`**: Runs tool in temporary virtual environment, cached but
  disposable
- **`uv tool install <tool>`**: Persistent installation, executables on PATH

**Key features**:

- Per-tool isolated virtual environments (no cross-tool conflicts)
- Version syntax: `uvx ruff==0.3.0` (pinned), `uvx ruff@latest` (latest only)
- Constraint preservation: `uv tool install black>=23,<24` is respected by
  `uv tool upgrade`

**Relevance to requirements**:

- **3.2 Dependency isolation**: Per-tool venvs achieve isolation. Our separate
  lock dirs provide similar isolation.

**Gap**: No project-local tool declaration. Tools are always user-global. This
violates our [requirement 1.2](#12-workspace-local) (Workspace-local).

**What we should adopt**:

- `upgrade` command that respects original constraints
- Ephemeral vs persistent distinction

CR-soon Alizter: `dune tools upgrade` is not in the CLI specification. Add it or
note as future work.

### cargo (Rust)

[cargo install][cargo docs] provides minimal built-in tool support:

- All binaries installed to single `~/.cargo/bin/` directory
- No isolation between tools
- No multi-version support (new version overwrites old)
- No project-local tools

**Key features**:

- Version constraint syntax: `cargo install ripgrep@1.2.0`, `--version ~1.2`
- Smart reinstall: only rebuilds if version/features/profile changed
- `cargo install --list` shows all installed packages

**Relevance to requirements**:

- **1.2 Workspace-local**: Violated. Cargo's model is entirely system-wide.
- **3.2 Dependency isolation**: Violated. All tools share one bin directory.

**Gap**: No project-local tools, which is why cargo-run-bin exists.

### cargo-run-bin (Rust community tool)

[cargo-run-bin] is the **closest analog to our design**. It fills cargo's gaps
with project-local tool management:

```toml
[package.metadata.bin]
cargo-nextest = { version = "0.9.57", locked = true }
dprint = { version = "0.30.3" }
cargo-mobile2 = { version = "0.5.2", bins = ["cargo-android", "cargo-mobile"] }
```

**Key features**:

- Project-local `.bin/` cache directory
- Declarative config in `Cargo.toml` metadata section
- `bins` array for multi-binary packages
- `locked` flag for reproducible dependency resolution
- `cargo bin --install` to install all configured tools at once

**Relevance to requirements**:

- **1.2 Workspace-local**: ✓ Achieves this via `.bin/` directory
- **4.2 Persistent configuration**: ✓ Uses `Cargo.toml` metadata

**What we should adopt**:

- `bins` field for explicit binary listing (vs auto-discovery)
- `--locked` flag for reproducibility
- **Batch install command** (`dune tools add`). Critical for CI

### npm (JavaScript)

[npm][npm docs] has the most mature approach for project-local tools:

- Tools declared in `devDependencies` in `package.json`
- Binaries linked to `node_modules/.bin/`
- `npx` runs binaries, preferring local over global
- npm scripts automatically get `node_modules/.bin` in PATH
- `package-lock.json` provides exact version pinning

**Relevance to requirements**:

- **1.2 Workspace-local**: ✓ `node_modules/.bin/` is project-local
- **2.2 Programmatic use**: ✓ Single directory for all tool binaries
- **4.2 Persistent configuration**: ✓ `devDependencies` in package.json

**What we should adopt**:

- devDependencies pattern (tools as dev deps, committed to repo)
- Auto-PATH for scripts (tools available without explicit path)
- `npx` prefers local, falls back to fetch

### How Our Design Compares

**Note**: Dune entries below describe **design requirements**, not necessarily
fully implemented features. Each entry references the relevant requirement
(e.g., "Req 1.2") to enable verification against the specification.

#### Tools compared

**uv** (Python): Fast Python package manager with tool support. Tools are
installed globally with per-tool isolated virtual environments. Supports
ephemeral runs (`uvx`) and constraint-preserving upgrades. No project-local or
declarative configuration. ([uv docs])

**cargo** (Rust): Rust's built-in package manager. Minimal tool support. All
binaries go to `~/.cargo/bin/`, no isolation, no multi-version. Smart reinstall
detects version changes. Compiler matching via rustup's `rust-toolchain.toml`.
([cargo docs])

**cargo-run-bin** (Rust): Community tool filling cargo's gaps. Project-local
`.bin/` cache, declarative config in `Cargo.toml` metadata, batch install.
Closest analog to our design in the Rust ecosystem. ([cargo-run-bin])

**npm** (JavaScript): Most mature project-local approach. Tools in
`devDependencies`, binaries in `node_modules/.bin/`, `npx` for execution.
Semver-respecting upgrades, lock file for reproducibility. ([npm docs])

#### Comparison table

| Feature              | dune | uv  | cargo | cargo-run-bin | npm |
| -------------------- | ---- | --- | ----- | ------------- | --- |
| Project-local        | ✓    | ✗   | ✗     | ✓             | ✓   |
| Multi-version        | ✓    | ✗   | ✗     | ✗             | ✓\* |
| Declarative config   | ✓    | ✗   | ✗     | ✓             | ✓   |
| Compiler matching    | ✓    | ✗   | ✓\*   | ✗             | ✗   |
| PATH fallback        | ✓    | ✗   | ✗     | ✗             | ✓   |
| Precompiled binaries | ✗    | ✓   | ✗     | ✓\*           | ✓   |
| Tool isolation       | ✓    | ✓   | ✗     | ✓             | ✓   |
| Ephemeral runs       | ✗    | ✓   | ✗     | ✗             | ✓   |
| Batch install        | ✓    | ✗   | ✗     | ✓             | ✓   |
| Binary selection     | ✓    | ✓   | ✓     | ✓             | ✓   |
| Tool discovery       | ✓    | ✓   | ✗     | ✗             | ✓   |
| Tool upgrade         | ✗    | ✓   | ✓\*   | ✗             | ✓   |
| System-wide install  | ✗    | ✓   | ✓     | ✗             | ✓   |

_Asterisks indicate caveats; see relevant subsection for details._

#### Project-local

Tools are installed within the project directory, isolated from other projects
and the global system.

**Dune** (Req 1.2, 1.5): Tools are workspace-local. Lock files stored in
`_build/.tools.lock/<pkg>/<version>/`, built artifacts in
`_build/_private/default/.tools/<pkg>/<version>/`.

**uv**: ✗ Tools installed globally. "Each tool environment is linked to a
specific Python version." Multiple Python versions require separate installs
with `--python` flag. If the Python version is uninstalled, the tool breaks.
([uv docs])

**cargo**: ✗ All binaries installed to single `~/.cargo/bin/`, shared by all
projects. Only one version per tool; reinstalling overwrites. Different versions
require custom `CARGO_INSTALL_ROOT` per project. ([cargo docs])

**cargo-run-bin**: ✓ Project-local `.bin/` cache directory. "Designed to manage
your Rust binaries within the context of your project." ([cargo-run-bin])

**npm**: ✓ Binaries linked to `node_modules/.bin/` per project. "Executables
linked into `./node_modules/.bin`." ([npm docs])

#### Multi-version

Multiple versions of the same tool can coexist and be used within a single
workspace.

**Dune** (Req 1.4): Versioned paths `_build/.tools.lock/<pkg>/<version>/` allow
multiple versions. Version selection: if 1 version locked, use it; if multiple,
error unless specified (e.g., `.ocamlformat` version field selects for
formatting).

**uv**: ✗ "Installing a tool with the same name will replace the existing tool."
Open feature request for per-directory pinning (GitHub #17813). ([uv docs])

**cargo**: ✗ "If the package is already installed, Cargo will reinstall it if
the installed version does not appear to be up-to-date." Only one version per
install root; use `--force` to explicitly switch versions. ([cargo docs])

**cargo-run-bin**: ✗ Single version per tool in `Cargo.toml` metadata.
([cargo-run-bin])

**npm**: ✓\* Nested `node_modules/` can contain different versions for
transitive dependencies. Package aliases allow explicit multi-version:
`npm install react15@npm:react@^15`. For CLI tools, `npx pkg@version` runs
specific versions. ([npm docs: folders])

#### Declarative config

Tools can be declared in a configuration file, enabling reproducible setups.

**Dune** (Req 4.2): `(tool)` stanza in `dune-workspace`:

```lisp
(tool
 (package
  (ocamlformat
   (= 0.26.2))))
```

**uv**: ✗ No project-level tool configuration. Tools installed via CLI only.
([uv docs])

**cargo**: ✗ No built-in declarative tool configuration. Feature request was
explicitly rejected as "NOT_PLANNED" - Cargo team directed users to third-party
tools instead. ([cargo#5120])

**cargo-run-bin**: ✓ Declared in `Cargo.toml` metadata section:

```toml
[package.metadata.bin]
cargo-nextest = { version = "0.9.57" }
```

([cargo-run-bin])

**npm**: ✓ Declared in `package.json` devDependencies:

```json
{ "devDependencies": { "prettier": "^3.0.0" } }
```

([npm docs: package.json])

#### Compiler matching

Tools are built with a compiler version matching the project to ensure ABI
compatibility.

**Dune** (Req 3.1): Automatic by default. Tools match project compiler or system
OCaml. Opt-out via `(skip_compiler_match)`.

**uv**: ✗ Tools ignore project-level Python version settings. "Tool environment
details: Each tool environment is linked to a specific Python version... but
will ignore non-global Python version requests like `.python-version` files and
the `requires-python` value from a `pyproject.toml`." ([uv docs])

**cargo**: ✓* Via rustup (not cargo itself): `rust-toolchain.toml` in the
*current directory* affects `cargo install`. Note: this uses *your project's\*
toolchain, not the tool's - a known issue ([rust-lang/cargo#11036]). ([rustup
docs])

[rust-lang/cargo#11036]: https://github.com/rust-lang/cargo/issues/11036

**cargo-run-bin**: ✗ Uses whatever Rust toolchain is active.

**npm**: ✗ No automatic matching. Packages declare Node version via `engines`
field, but npm only warns by default (requires `.npmrc` `engine-strict=true` to
enforce). Native addons use ABI version matching via node-pre-gyp. ([npm docs:
package.json])

#### PATH fallback

When a tool is not installed via the tool manager, fall back to system PATH.

**Dune** (Req 2.4): Build rules (e.g., `dune fmt`) fall back to system PATH when
no tool is locked. Behavior for `dune tools run` is an open question (see
below).

**uv**: ✗ Does not search system PATH for tools.

**cargo**: ✗ `cargo install` manages `~/.cargo/bin/`, but individual commands
don't fall back to system PATH.

**cargo-run-bin**: ✗ Only runs binaries from local `.bin/` cache.

**npm**: ✓ npx checks `$PATH` and `node_modules/.bin` before downloading. Use
`--ignore-existing` to skip PATH lookup. (Note: "shell fallback" was a different
feature—`--shell-auto-fallback`—removed in npm v7.) ([npm v7 changes])

#### Precompiled binaries

Tools can be downloaded as precompiled binaries rather than built from source.

**Dune** (Non-req 6.5): ✗ Tools always built from source. Ensures compiler
compatibility and avoids ABI issues.

**uv**: ✓ Downloads prebuilt wheels when available. "uv downloads and installs
packages from PyPI." ([uv docs])

**cargo**: ✗ `cargo install` compiles from source. ([cargo docs])

**cargo-run-bin**: ✓\* Optional binstall support. "Download pre-compiled
binaries using cargo-binstall." ([cargo-run-bin])

**npm**: ✓ Packages are prebuilt JavaScript. Native addons may require
compilation.

#### Tool isolation

Each tool's dependencies are isolated from other tools and from project
dependencies.

**Dune** (Req 3.2): Each tool solved independently with its own lock directory
in `_build/.tools.lock/<pkg>/<version>/`.

**uv**: ✓ "Each tool has its own isolated virtual environment." ([uv docs])

**cargo**: ✗ All tools share single `~/.cargo/bin/` directory. No dependency
isolation between tools.

**cargo-run-bin**: ✓ Each tool installed to separate directory under `.bin/`.
([cargo-run-bin])

**npm**: ✓ Each project has isolated `node_modules/`. Tools share dependencies
within a project but are isolated between projects.

#### Ephemeral runs

Run a tool once without persisting its installation - useful for trying tools or
running one-off commands without polluting the workspace's tool configuration.

**Dune** (Non-req 6.6): ✗ Tools must be locked before running. Building from
source is slow, so caching (locking) is more practical than discarding. Users
can `dune tools remove` after if needed.

**uv**: ✓ `uvx` runs in temporary environment. "Runs a tool without installing
it... The tool environment is cached." ([uv docs: uvx])

**cargo**: ✗ No ephemeral execution; `cargo install` always persists.

**cargo-run-bin**: ✗ Tools must be configured and installed first.

**npm**: ✓ `npx` can fetch and run without adding to package.json. "Run a
command from a local or remote npm package." ([npx docs])

#### Batch install

Install all declared tools with a single command.

**Dune** (Req 4.1): `dune tools add` with no arguments locks all tools from
`(tool)` stanzas.

**uv**: ✗ No batch install; tools installed individually via `uv tool install`.

**cargo**: ✗ No declarative tool configuration, so no batch install.

**cargo-run-bin**: ✓ `cargo bin --install` installs all tools from config.
"cargo bin --install - Installs all binaries." ([cargo-run-bin])

**npm**: ✓ `npm install` installs all devDependencies. "Install all modules
listed as dependencies in package.json." ([npm docs])

#### Binary selection

When a package provides multiple executables, how to specify which one to run.

**Dune** (Req 1.6): `--bin` flag on `dune tools run` and `dune tools path`.
Auto-selects if package provides single binary; errors with helpful message if
multiple binaries exist. Declarative via `(executable ...)` in `(tool)` stanza.

**uv**: ✓ `--from` option specifies package when command differs from package
name: `uvx --from httpie http`. Without `--from`, uvx assumes command name
equals package name. `uv tool install` installs all binaries from a package.
([uv docs])

**cargo**: ✓ `--bin NAME` flag selects specific binary. Installs all binaries by
default. `default-run` in Cargo.toml only affects `cargo run`, not install.
Binary name conflicts between packages cause errors. ([cargo docs])

**cargo-run-bin**: ✓ `bins` array in config specifies which binaries to build:
`cargo-mobile2 = { version = "0.5.2", bins = ["cargo-android", "cargo-mobile"] }`.
Builds all binaries if `bins` omitted. ([cargo-run-bin])

**npm**: ✓ `--package` (or `-p`) specifies package when binary name differs:
`npx --package=foo bar`. npx auto-matches binary to package name; errors if no
match. `bin` field in package.json maps command names to scripts. ([npx docs])

#### Tool discovery

How scripts and editors programmatically find paths to installed tool
executables.

**Dune** (Req 2.2): `dune tools path <tool>` returns the executable path for a
specific tool. Per-tool granularity enables editors to find exact binaries.

**uv**: ✓ `uv tool dir --bin` returns the bin directory (e.g., `~/.local/bin`).
`uv tool list` shows installed tools. No per-tool path command; construct path
as `$(uv tool dir --bin)/<tool>`. ([uv docs])

**cargo**: ✗ No discovery command. Binaries in `~/.cargo/bin/` (or
`$CARGO_INSTALL_ROOT/bin`). `cargo install --list` shows installed crates but
not paths. Scripts use `which <tool>` or hardcode `~/.cargo/bin/<tool>`. ([cargo
docs])

**cargo-run-bin**: ✗ Binaries cached in `.bin/` directory. No CLI path command;
library API (`binary::install()`) returns paths programmatically.
([cargo-run-bin])

**npm**: ✓ `npm bin` returns `node_modules/.bin` directory (removed in npm 9+).
npm scripts automatically get `.bin` in PATH. No per-tool path command; use
`$(npm bin)/<tool>` or `./node_modules/.bin/<tool>`. ([npm docs: bin])

#### Tool upgrade

Update installed tools to newer versions, optionally preserving version
constraints.

**Dune** (Non-req 6.2): ✗ No upgrade command. Re-run `dune tools add <pkg>` to
get latest version, or `dune tools add <pkg>.<version>` for specific version.
Constraints from `(tool)` stanza preserved on re-lock.

**uv**: ✓ `uv tool upgrade <tool>` or `--all`. Respects original constraints: if
installed with `black>=23,<24`, upgrade stays within that range. To change
constraints, must reinstall with `uv tool install`. ([uv docs])

**cargo**: ✓\* `cargo install <pkg>` auto-detects newer versions without
`--force`. No constraint preservation; always installs latest (or specified)
version. Third-party `cargo-update` provides `cargo install-update -a` for batch
upgrades. ([cargo docs])

**cargo-run-bin**: ✗ No upgrade command. Edit version in `Cargo.toml` manually,
then run `cargo bin --install`. Version-controlled config ensures team sync.
([cargo-run-bin])

**npm**: ✓ `npm update` upgrades within semver constraints (e.g., `^1.2.3`
allows minor/patch updates). `npm outdated` shows available updates. For major
version bumps beyond constraints, use third-party `npm-check-updates`. ([npm
docs: update])

#### System-wide install

Install tools globally, available outside any specific project.

**Dune**: ✗ Intentionally omitted. Tools are workspace-local only. (See
[Non-requirement 6.1](#61-system-wide-installation))

**uv**: ✓ `uv tool install` adds to PATH. "Executables are installed to the uv
tools directory." ([uv docs])

**cargo**: ✓ `cargo install` puts binaries in `~/.cargo/bin/`, typically on
PATH. ([cargo docs])

**cargo-run-bin**: ✗ Project-local only.

**npm**: ✓ `npm install -g` installs globally to system. ([npm docs])

### Recommendations from Research

CR-soon Alizter: Review the entire Comparison with Other Tools section for
accuracy and completeness. Verify citations are still valid and that the
comparison table reflects current design decisions.

**Adopted in this design**:

- **Batch install** (from cargo-run-bin, npm): `dune tools add` with no
  arguments locks all tools from `(tool)` stanzas (Req 4.1)
- **Declarative config** (from npm, cargo-run-bin): `(tool)` stanza in
  `dune-workspace` (Req 4.2)
- **Binary selection** (from cargo-run-bin): `(executable ...)` field and
  `--bin` flag (Req 1.6)

**Future consideration**:

- **`--locked` flag** (from cargo-run-bin): Ensure exact versions from solver,
  not just constraints
- **Ephemeral run mode** (from uv's `uvx`): See
  [Non-requirement 6.6](#66-ephemeral-runs)
- **System-wide linking** (from uv): See
  [Non-requirement 6.1](#61-system-wide-installation)

[uv docs]: https://docs.astral.sh/uv/concepts/tools/
[uv docs: uvx]: https://docs.astral.sh/uv/concepts/tools/#running-tools
[cargo docs]: https://doc.rust-lang.org/cargo/commands/cargo-install.html
[cargo#5120]: https://github.com/rust-lang/cargo/issues/5120
[rustup docs]: https://rust-lang.github.io/rustup/overrides.html
[cargo-run-bin]: https://github.com/dustinblackman/cargo-run-bin
[npm docs]: https://docs.npmjs.com/cli/
[npm docs: folders]: https://docs.npmjs.com/cli/v9/configuring-npm/folders
[npm docs: package.json]:
  https://docs.npmjs.com/cli/v10/configuring-npm/package-json
[npm docs: bin]: https://docs.npmjs.com/cli/v8/commands/npm-bin
[npm docs: update]: https://docs.npmjs.com/cli/v10/commands/npm-update
[npx docs]: https://docs.npmjs.com/cli/v10/commands/npx
[npm v7 changes]:
  https://blog.npmjs.org/post/626173315965468672/npm-v7-series-beta-release-and-semver-major.html

## Learnings from the Prototype

CR-soon Alizter: This entire section may be considered garbage. Extract
important information out into the spec or remove entirely.

This section documents key insights from the initial prototype implementation.

### What Worked Well

1. **Versioned lock directories**: Storing tools in
   `_build/.tools.lock/<pkg>/<version>/` cleanly separates versions and keeps
   the source tree clean. The flow (solve → extract version → move to versioned
   path) is straightforward.

2. **Tool isolation via synthetic wrapper**: Solving tools with a synthetic
   `<pkg>_tool_wrapper` package (not mixed with project packages) prevents
   "packages outside workspace" solver errors.

3. **Install cookie as build target**: Depending on `target/cookie` rather than
   individual binaries correctly handles directory targets. The cookie is
   created after full installation, providing a reliable completion signal.

4. **Disk reads for lock checks**: Reading lock directories directly from disk
   (not via build system) avoids triggering expensive project builds during tool
   commands.

5. **Simple PATH-only environment**: Tools only need the bin directory in PATH,
   not a full exported environment. This avoids expensive closure computations.

6. **Unified resolution with fallback**: A single resolution chain (stanza →
   locked → PATH) provides clear priority and graceful degradation.

### Problems Discovered

1. **Directory targets cannot be depended upon directly**: Naively depending on
   `_build/.../target/bin/<exe>` fails. Solution: depend on cookie, then read
   cookie to discover binaries.

2. **Build system triggers on lock checks**: Calling `Lock_dir.get` triggered
   full builds. Solution: use `Lock_dir.read_disk` for checking.

CR-soon Alizter: Need to understand what this is about.

3. **Checksum collection missed versioned paths**: Fetch rules initially only
   collected unversioned paths. Solution: scan `.tools.lock/` for all versions.

4. **`.pkg/` rules gated by project lock**: Initial design skipped rule
   generation when no project lock existed, breaking tools-only projects.
   Solution: always generate rules; create tools-only DB when needed.

5. **Compiler matching requires explicit package**: Just constraining `ocaml`
   version wasn't enough; solver picked `ocaml-base-compiler` instead of
   `ocaml-system`. Solution: explicitly require `ocaml-system` when system
   compiler is available.

### Reference Packages for Testing

The implementation should be tested against these packages. We don't
special-case them, but they represent common tool patterns and historic
dev-tools support.

CR-soon Alizter: Vet this list. Are there other common tools to add? Any that
should be removed? Consider tools from opam-repository's most-installed
packages.

**Historic dev-tools** (from `src/dune_pkg/dev_tool.ml`):

| Package            | Binary           | Notes                             |
| ------------------ | ---------------- | --------------------------------- |
| `ocamlformat`      | `ocamlformat`    | Also has `ocamlformat-rpc` binary |
| `odoc`             | `odoc`           | Documentation generator           |
| `ocaml-lsp-server` | `ocamllsp`       | Binary name differs from package  |
| `utop`             | `utop`           | REPL, needs project libraries     |
| `earlybird`        | `ocamlearlybird` | Debugger                          |
| `odig`             | `odig`           | Documentation viewer              |
| `opam-publish`     | `opam-publish`   | Release tooling                   |
| `dune-release`     | `dune-release`   | Release tooling                   |
| `ocaml-index`      | `ocaml-index`    | Indexer for ocamllsp              |
| `merlin`           | `ocamlmerlin`    | Editor support                    |

CR-soon Alizter: These are not historic dev tools, these are dev tools that we
currently support and would like to continue to support. Linking this to the
migration requirement. They will serve as test cases sure, but they also serve
as platform roadmap integration etc.

**Edge cases to verify**:

- Package with single binary: auto-selection works
- Package with multiple binaries: error with helpful message
- Package name ≠ binary name: `--bin` or `(executable)` works
- Tool needing project context: `utop` with `#require`

CR-soon Alizter: I don't like these verifications here, they are out of place
