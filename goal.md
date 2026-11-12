# Experimental `opam` stanza

## Goal

As part of the package-management overhaul, an already selected package source
is loaded into the workspace and interpreted either as native Dune stanzas or
as one opaque Opam build.

This document specifies the Opam interpretation, which runs the package recipe
in a writable copy sandbox and records the resulting package contents,
libraries, executables, variables, and exported environment.

The unreleased `opam` stanza is only a test entrance to this rule generator.

## Parent-declared opaque directories

An `opam` stanza declares its opaque directory from the parent, using a required
`dir` field:

```lisp
(opam
 (package geometry)
 (dir geometry)
 (version 1.0)
 (source .)
 ...)
```

`dir` is a literal immediate child name relative to the stanza's effective
location. Empty names, `.`, `..`, absolute paths, multi-component paths, and
variable expansions are rejected. The name is not interpreted as a glob. Two
Opam stanzas cannot claim the same child directory.

Static `include` and `subdir` placement follow normal Dune directory semantics.
For example, `(subdir geometry (opam (package vendor) (dir vendor) ...))` selects
`geometry/vendor`, not `geometry`. Relative `source` and `files` inputs are
resolved from the selected child directory. The optional `source` field
controls whether a primary source tree is copied; `dir` alone does not imply
one.

The named child gets `data_only_dirs` semantics before it is traversed. Its
`dune` and `dune-project` files, and those in its descendants, are source data
rather than native configuration. The parent registers the Opam recipe. Other
parent declarations, including explicit `subdir` declarations, are interpreted
normally.

Declaring the child in the parent gives the loader its directory status before
traversal. This uses ordinary data-only directory handling and allows selecting
Opam interpretation without reading or editing the supplied package's own
build configuration. Directory classification belongs to source loading, not
the shared executor.

## Constraints

Opam rule generation receives the exact selected package identity and metadata,
prepared source inputs and their build dependencies, the complete recipe,
materialized dependency capabilities and their build dependencies, context,
working and output locations, and artifact owner. The recipe includes build
and install actions, depexts, and exported environment updates. The executor
does not solve dependencies, rediscover ownership, or construct a second
package graph.

Opam rule generation is not supported in cross-compilation target contexts.
Requests in such contexts are rejected.

Selected package identity is separate from exposed variables. Default version
and development status come from the supplied metadata, not inference from the
package name or version string. Variables from an opam `.config` file may
override exposed values without changing the selected identity.

An Opam build owns one persistent internal install tree and a cookie
describing installed files by section and package variables. These are the
executor's build outputs; it does not produce another package database.

The producer's install tree is distinct from a consumer's package install
layout. Its install prefix is independent of consumer dependency sets. Shared
materialization exposes the resulting outputs in consumer layouts; creating
another layout does not itself rerun the Opam build or install recipe.

Exposed section paths and exported paths to installed contents remain usable
after the build sandbox is removed. This does not require arbitrary installed
binaries to be relocatable.

Native and Opam packages expose the same logical capabilities even though
native outputs use fine-grained rules and Opam outputs cross an opaque cookie
boundary.

A rule materializes packages through explicit `(package ...)` dependencies. Each
package name resolves in the rule's build context to the selected native Dune or
Opam package, and its outputs are exposed through the normal package install
layout as build dependencies, installed-file lookups, installed library
metadata, an executable map, package variables, and environment updates.

Package ownership attributes a rule's outputs; it does not implicitly add
packages to the rule's materialized dependency set.

Installed contents are addressed through explicit package and section lookups,
libraries use normal library resolution, and executables use normal binary
resolution and `PATH` construction.

A Dune rule's explicit `(package ...)` dependencies determine the package
capabilities added to its action layout. They do not replace normal Dune
library and binary lookup or exclude existing context tools. Declared package
dependencies (`depends`) remain available to narrow searches, including those
in the artifacts database. Search narrowing is distinct from materialization;
it does not implicitly add packages to a rule's materialized dependency set.

Explicit installed-file and package-variable lookups in an opaque Opam build
use the direct dependencies supplied to its rule generator. Binary lookup and
`PATH` construction use their transitive dependency closure through the shared
narrowing machinery. Dependencies are built as required; the executor does not
construct another package graph.

An Opam package's package-dependency action builder records and returns the
package's declared environment updates as data, preserving declaration order.
It does not apply the package's exports to the consumer's environment; the API
consumer is responsible for combining and applying the returned updates.

Native library stanzas consume installed libraries through normal
`(libraries ...)` resolution, separately from an action's explicit
`(deps (package ...))` layout.

Capability lookup is keyed by exact package identity and build context rather
than a process-global package-name table.

Source inputs are ready to build. Fetching, patching, package-file copies, and
extra-source overlays are completed by the source-loading component before
the `opam` stanza sees them. The executor does not perform source preparation.
Workspace and prepared package sources remain immutable during the opaque
build.

The child named by an `opam` stanza's `dir` field and its descendants are
data-only for native configuration loading, as with `data_only_dirs`. The
stanza is recognized in the parent; the named child's own configuration is not
read to discover the recipe.

A package without a primary source consumes ready-to-build files from the
caller, including any prepared package files and extra sources. With no files,
its sandbox working directory starts empty. It produces the install tree and
cookie without creating a primary-source target.

## Tests

### Rule generation

- Test that using the parsed `opam` stanza without the unreleased extension is
  rejected.
- Test that requesting an Opam build in a cross-compilation target context is
  rejected as unsupported.
- Test that `dir` is required, denotes one literal immediate child, and rejects
  invalid names and duplicate directory claims.
- Test that the named child and its descendants have `data_only_dirs`
  semantics, including ignoring malformed `dune` and `dune-project` files.
- Test that parent and sibling native rules generate their expected outputs,
  and that static `include` and `subdir` placement select the correct opaque
  child.
- Test that explicit parent declarations and `data_only_dirs` settings coexist
  with Opam directory declarations.
- Test that relative source inputs resolve from the selected child, and that
  changing `dir` updates both source discovery and the opaque boundary.
- Test that build and install actions consume caller-prepared sources,
  including patched, overlaid, and executable files, from a writable copy
  without modifying the supplied sources or repeating preparation.
- Test that a package without a primary source consumes caller-prepared files
  and produces the install tree and cookie without a primary-source target.
- Test that a source-less package with no files or actions still uses an empty
  writable sandbox and produces the install tree and cookie.
- Test that action expansion preserves the supplied version and development
  status, including a package with a released version and `dev = true`.
- Test that a failed action reports the package identity and preserves its build
  log.

### Package outputs

- Test that files created by an install action are recorded in the cookie under
  the correct install sections.
- Test that entries from an opam `.install` file are combined with files created
  by the install action.
- Test that variables from an opam `.config` file are recorded in the cookie
  and can override the exposed `version` without changing the selected package
  identity.
- Test that installed library metadata is exposed as package libraries.
- Test that files installed in the binary section are exposed as package
  executables.
- Test that the package-dependency action builder returns the declared
  environment updates in declaration order without applying them.
- Test that changing the installed output replaces removed files in the package
  capability set.

### Capability consumption

- Test that an explicit `(package ...)` rule dependency resolves to an Opam
  package and materializes its outputs in the rule's package install layout.
- Test that a rule with both native and Opam package dependencies can read an
  Opam-installed data file by package, section, and relative path, and through
  an exported path value, after the producer's build sandbox is removed.
- Test that consumers in one context with different package dependency sets
  reuse the same Opam install tree and cookie without rerunning its recipe.
- Test that a rule reading an installed file records a dependency on the
  producing package output.
- Test that a native library can resolve and link an Opam-produced library
  through normal `(libraries ...)` resolution.
- Test that `%{bin:...}` resolves an executable produced by an explicit Opam
  package dependency.
- Test that an action's `PATH` contains executables from its explicit package
  dependencies.
- Test that a consuming action can apply environment updates returned by an
  explicit Opam package dependency.
- Test that package variables from an explicit package dependency are available
  during action expansion.
- Test that an Opam package can consume capabilities produced by a native Dune
  package.

### Dependency visibility

- In a package chain `A -> B -> C`, test that `A`'s opaque build can use
  installed files, executables, and variables from `B`.
- Test that `A` can resolve and run an executable from `C` through transitive
  binary lookup and `PATH` construction.
- Test that `C` is not available through `A`'s direct installed-file or
  package-variable lookups.
- Test that `C` is still built when required to build `B`, without exposing
  `C` through those direct lookups.
- Test that an unrelated package is neither visible nor built.
- Test that conflicting library or executable providers produce an ambiguity
  diagnostic.

### Invalidation and isolation

- Test that requesting a capability builds the producing package and its cookie.
- Test that changing package sources or the recipe rebuilds the opaque package.
- Test that changing a producer's installed files or configuration variables
  regenerates its cookie and invalidates affected consumers.
- Test that a consumer with an explicit `(package ...)` dependency rebuilds
  and reads updated installed-file contents even when the cookie remains
  byte-for-byte identical.
- Test that capabilities from different exact package identities do not leak
  into each other.
- Test that capabilities from different build contexts do not leak into each
  other.
- Test that sharing the executor preserves existing lock-package build,
  sandbox, install, configuration, and depext behavior.

## Out of scope

Package solving, source selection and preparation, Dune-versus-Opam
classification, and native Dune rule generation are specified by the
surrounding package-management design rather than by this stanza.

Consumer install-layout construction and package capability forwarding belong
to shared package materialization, not to the Opam executor.

Required-library closure handling belongs to the shared `(package ...)`
dependency mechanism and normal library resolution. This goal assumes that
machinery supplies the correct closure rather than specifying or testing its
closure policy.

Environment composition, including dependency-closure propagation, expansion,
ordering, and action-local overrides, belongs to API consumers and the shared
package-dependency machinery. This goal specifies the returned updates, not a
separate composition policy.

---

# Document review

## Proposed Edits

Open decisions are listed below under their original proposal numbers. Applied
parts are noted explicitly. The existing rejected notes below are unchanged.

### P4. Scope of Transitive Opam Narrowing

Ali: Let's do transitive narrowing because opam is special like that.

Applied: Binary lookup and `PATH` for opaque Opam builds use the transitive
dependency closure through the shared narrowing machinery. Ordinary explicit
`(package ...)` materialization is unchanged.

Question: Does transitive narrowing also apply to installed-file and
package-variable lookups in opaque Opam builds, or only to binaries? Their
previous direct-only policy is retained until this scope is confirmed.

### P12. Use Separate Collision Policies

Replace the blanket library/executable ambiguity test with install-destination
collision tests using the existing install-layout conflict policy, including
native/opaque combinations. Outside those destination conflicts, retain normal
library ambiguity checks and binary lookup precedence rather than introducing
one new shadowing or ambiguity rule for all capabilities.

This is a proposed resolution of the provider-ambiguity note below, not a
change to that note's current status.

### P13. Bound Integration Scope and Name Regression Coverage

Keep solving, selection, native loading, multi-version integration, and removal
of the legacy package runtime outside this executor task. Preserve exact
identity and context in the interface without making a complete multi-version
workspace implementation a prerequisite.

Replace the single "preserves existing lock-package behaviour" test bullet with
a regression checklist naming the existing lock-package sandbox, install-file,
configuration, build-log, and depext tests. Record intentional visibility and
environment changes separately rather than asserting unchanged behaviour for
those features.

### P14. Add One End-to-End Acceptance Scenario

Add a native producer supplying an opaque recipe, followed by an ordinary rule
with an explicit package dependency consuming the opaque result. Demonstrate
source immutability, installed-file/binary/variable consumption, and consumer
invalidation after a producer change. Use this to connect the individual tests,
not as another broad visibility requirement or an ownership-derived dependency.

## Rejected

--- 

TODO: do not edit yet


Unsettled semantics                                                                                                                    
- Provider ambiguity — The test requiring an ambiguity diagnostic assumes
collisions are errors. We have not decided whether           executable and
library collisions should error, shadow, or use dependency order.                                                     

- Exact package identity — Isolation between exact identities is an
overhaul-level invariant. This branch can avoid global name-based  state, but
testing multiple exact versions likely belongs to the integration branch.                                                 
                                                                                                                                        
Redundant or poorly formed tests                                                                                                       
- Installed-file lookup and dependency recording — One test can read the file,
then change the producer and prove the consumer         rebuilds.                                                                                                                            
- Direct capability visibility — The broad visibility tests duplicate the
individual file, binary, variable, and environment           consumption tests.                                                                                                                   
- “Preserves existing lock-package behavior” — This is a regression objective,
not one executable test case. It should name existing   tests to rerun or
enumerate specific preserved behaviors.                                                                            
- “Unrelated package is neither visible nor built” — This combines visibility
with demand-driven building; those are distinct          behaviors and may
belong to different layers.                                                                                        


