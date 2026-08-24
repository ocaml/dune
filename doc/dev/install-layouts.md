# Install Layouts for Package and Library Sets

## Overview

`(deps (package ...))` materializes a scoped install layout under
`_build/install/<context>/.packages/<digest>/` containing the declared packages
and the workspace library artifacts needed by their OCaml and Melange library
closures.
This replaces the old alias-based mechanism where `(deps (package foo))`
depended on the `.foo-files` install alias, which populated the
`_build/install/` staging area shared by all packages.

## Motivation

The `_build/install/` staging area shared by all packages causes several
problems:

1. Actions can silently depend on arbitrary packages they did not declare via
   the shared environment variables (OCAMLPATH, PATH, etc.). Whether an action
   succeeds can depend on what other packages happened to be built, making
   builds non-deterministic. `(strict_package_deps)` validates that dependencies
   are declared but does not prevent unrelated packages from being visible at
   runtime.

2. The shared staging area can cause rule collisions and dependency cycles.
   Multiple packages installing directory targets to the same section root
   crash ([#13307]). Computing install artifacts globally creates cycles in
   monorepo builds ([#13500]) and Coq compositions ([#7908]).

3. Lock-dir packages that depend on workspace packages need a scoped install
   prefix to build against. The shared staging area does not provide this. This
   is the "in-and-out" problem ([#8652]): when a lock-dir package depends on a
   workspace package which depends on another lock-dir package, dune cannot
   resolve the dependency without per-package install layouts.

[#7908]: https://github.com/ocaml/dune/issues/7908
[#8652]: https://github.com/ocaml/dune/issues/8652
[#13307]: https://github.com/ocaml/dune/issues/13307
[#13500]: https://github.com/ocaml/dune/issues/13500

## Design

### Package kinds

`Package_db.find_package` classifies packages into three kinds:

- `Local`: workspace packages defined in `dune-project`. These get layout
  file deps and env vars.

- `Build`: lock-dir packages from `dune.lock`. `(deps (package foo))` runs
  the package's build action but does not set up env vars for the consuming
  action. This is a known bug, tracked as a follow-up in PR #14373.

- `Installed`: externally installed packages (found via findlib). Depended
  on directly. Already on the system OCAMLPATH. Only returned in
  non-lock-dir contexts; with a lock-dir active, external packages can
  only resolve as `Build` (if they are in `dune.lock`) or as not found.

### Action package dependencies versus package metadata

Two different dependencies are involved here:

- `(deps (package foo))` is an action dependency. It asks Dune to build or
  locate the installable contents of `foo`, make them available to the action,
  and track those contents as inputs.

- `(package (name foo) (depends ...))` records package metadata. It describes
  what must be available when `foo` is built or installed, but it does not say
  which artifacts an arbitrary action intends to use.

The install layout never computes the transitive closure of the second kind.
For example, if package `foo` has a package dependency on `bar`, then
`(deps (package foo))` does not expose `bar`'s executable, data files, or
unrelated libraries. An action using those must also declare
`(package bar)`. The only implicit expansion is the narrower OCaml library
closure described below.

### Library closure, not package closure

The layout starts with the packages listed in `(deps (package ...))`. For each
workspace package, Dune also finds the libraries it installs, computes their
transitive OCaml and Melange library closures, and adds only the install entries
belonging to those workspace libraries. It does not add the libraries' complete owning
packages or traverse dependencies in package metadata.

This distinction preserves two important properties:

1. Installed and workspace packages have the same library semantics. The
   `META` or `dune-package` entry for an installed library records the other
   libraries it requires, and OCaml tools recursively resolve those libraries.
   A scoped workspace layout must therefore make the same libraries findable.
   Otherwise, metadata in the layout can refer to libraries outside it and a
   command that works after installation can fail in the workspace.

2. Package closure cannot be defined consistently. Dune knows package
   dependencies for workspace and lock-directory packages, but `dune-package`
   does not record them and findlib packages do not provide a reliable package
   dependency graph. Following package dependencies only when that information
   happens to be available would make `(package ...)` depend on where a package
   came from.

Package dependencies are also broader than the requirement imposed by OCaml
library metadata: they can bring in tools, data, test dependencies, and
unrelated libraries. Such dependencies must still be listed explicitly when an
action uses them.

Dune starts from both the libraries and deprecated-library redirect targets
installed by each declared package. It then traverses each root library's
resolved `requires`, PPX runtime dependencies, and any virtual library default
implementation independently rather than trying to link all the package's
libraries together. A package may provide independent libraries or competing
implementations of a virtual library, so its libraries need not form one valid
link-time closure. Dune requires a public virtual library and its default
implementation to belong to the same package, but they remain separate
libraries and both are selected when the default is needed. Resolution errors
are reported rather than silently producing an incomplete layout. Every
intermediate name in a deprecated-library redirect chain is retained, including
metadata emitted under deprecated package names.

Library metadata is normally grouped by package. For each owning package in
the support closure, Dune therefore generates a filtered `META` and
`dune-package` containing only the selected libraries and relevant deprecated
library redirects. The layout similarly contains only those libraries'
interfaces, archives, runtime files, headers, and stubs. Library references
serialized by inline-test backends, PPX drivers, and instrumentation backends
are also included. Unrelated
sibling libraries and non-library entries such as binaries, data, and
documentation are excluded.

At namespace nodes that do not themselves contain a selected library, the
filtered `META` retains only `directory`, which is inherited by findlib
subpackages. Other template variables at such a node describe that package
node rather than its selected descendants and are dropped.

A META file template can replace dependency or artifact rules independently of
Dune's library graph and selected install entries. Such templates remain valid
for ordinary installation, but a scoped package layout rejects changes to
dependency and artifact-bearing variables in the selected package hierarchy.
Rules in unselected support subpackages are filtered first, while explicitly
requested packages are checked without filtering.

Package sites and their directories are also excluded. The filtered
`dune-package` consequently uses an empty sites map instead of advertising
paths that the support layout does not materialize.

The consuming action depends on every path materialized in the layout. A
support library therefore adds dependencies on its selected interfaces,
archives, stubs, headers, runtime files, and filtered metadata—not merely an
`OCAMLPATH` entry. The symlink or generated-file rule for each layout path then
tracks the corresponding workspace artifact or metadata input. Changes in a
transitively required workspace library consequently invalidate the action.

Installed libraries in the closure are already visible through the context
environment; only workspace library artifacts need adding to the scoped
layout. Because rule actions are opaque, a support library on `OCAMLPATH` is
also directly queryable with `ocamlfind`. The distinction is one of dependency
selection and layout contents, not an access-control boundary.

### Environment variables

The layout sets PATH, OCAMLPATH, CAML_LD_LIBRARY_PATH,
OCAMLFIND_IGNORE_DUPS_IN, OCAMLTOP_INCLUDE_PATH, and MANPATH from the layout's
directory structure. These are defined by `Install.Roots.path_vars` and consed
onto the directory environment in `extend_action` (super_context.ml), so
external/system paths remain visible. The env vars are propagated to rule
actions, cram tests, cinaps, mdx, and inline tests.

### Staging area

`(deps (package ...))` no longer populates the `_build/install/` staging area.
The staging area is only populated by `dune build @install` / `dune install`.
Code that previously relied on a binary being present in the staging area has
two replacements: either explicitly depend on `@install` to populate the
staging area, or use `%{bin:foo}`, which goes through the separate bin-layout
(`.binaries/<digest>/`) added in PR #14432 and bypasses the staging area
entirely.

### Environment hermeticity

The `env` lazy in `Super_context.create` (`src/dune_rules/super_context.ml`) is
just `Context.installed_env` plus `Site_env.add_packages_env`, with no staging
paths attached. Actions only see the named packages and the selected artifacts
of their library closure via layout env vars consed in `extend_action`. This is
what makes the strict-deps property hold for workspace build outputs: no
unrelated workspace-built artifact is reachable via dune-managed env vars.
The user's inherited shell env (PATH, OCAMLPATH, findlib config) is still
visible via `Context.installed_env`, as documented in "Environment variables"
above.

### `dune exec` and the staging area

`dune exec` runs binaries outside the rule machinery, so it cannot inherit
per-action env from `combined_package_deps_builder`. To preserve the documented
`dune exec` contract that workspace libraries are reachable via OCAMLPATH (see
the man page: "you will have access to the libraries defined in the workspace
using your usual directives"), `bin/exec.ml` conses
`_build/install/<context>/{lib,bin,...}` onto the path-like env vars of the
executed process.

This cons is unconditional and does not trigger any build. If `dune build
@install` (or any rule that drags install entries in) was run beforehand, the
staging dir is populated and workspace libraries are visible to anything the
binary delegates to (findlib, ocamlfind, dune-site plugin lookup, etc.). If
not, the entries on the path simply don't resolve. Per-action rule envs are
unaffected; only `dune exec`'s own env extension is consed.

### Sites and plugins

The dune-site mechanism uses `DUNE_DIR_LOCATIONS` to tell binaries where their
sites live. `Site_env.add_packages_env` walks workspace stanzas to discover
site/plugin packages and sets the env var to point at the staging area, one
entry per (package, section) pair. The path for a given section follows opam's
layout: `_build/install/<context>/<section>/<pkg>/` (e.g. `lib/<pkg>/` for the
`lib` section, `share/<pkg>/` for `share`). Combined with the `dune exec`
staging cons above, this means:

- A binary built locally and run via `dune exec foo.exe` sees plugins iff
  the staging dir is populated. The dune-site / sites cram tests
  consequently run `dune build @install` before `dune exec`.
- Cram tests that exercise dune-site libraries (`(libraries dune-site
  dune-site.plugins)`) can declare `(package dune-site)` in their cram-level
  `dune` setup. `dune-site` re-exports `dune-private-libs.dune-section`, so the
  library closure also adds that library's install artifacts and filtered
  metadata. It does not add the other contents of `dune-private-libs`. The same
  rule applies to other re-exporting libraries. Package dependencies that are
  not represented in the library graph must still be declared explicitly.

The current staging cons is sufficient for the existing test surface and
matches pre-install-layouts behaviour.

### Package set structure and `_root` section collisions

The layout merges the explicit packages' install entries and the selected
support libraries' entries into a single directory tree. For scoped sections
(`lib`, `share`, `doc`, `etc`), entries install under their owning package's
subdirectory (`lib/<pkgname>/`), so collisions are impossible by construction.

Collisions can only occur in `_root` sections (`lib_root`, `share_root`,
`libexec_root`), which install directly to the section root without package
namespacing ([opam manual][opam-install-format], [opam#2153]). In opam,
`_root` sections serve specific use cases:

- `ocamlfind` installs `topfind` into the compiler's stdlib directory via
  `lib_root` ([opam#45]).
- Cross-compiled C libraries install `.pc` files to `lib/pkgconfig/` via
  `lib_root`.
- `share_root` is used for shared data directories (e.g. emacs site-lisp).

These are rare. Collisions in `_root` sections are currently rejected with a
`User_error` naming both conflicting packages and the destination section/file
(see `compute_entries` in `src/dune_rules/install_layout.ml`, pinned by
`test/blackbox-tests/test-cases/package-materialization/root-section-collision.t`).
To support opam-style override semantics in the future, the layout would
resolve `_root` collisions using dependency edges: if B depends on A, B's
`_root` entries take priority. Diamond collisions (incomparable packages) would
remain errors. This only affects `_root` handling and does not require changing
the core layout mechanism.

[opam-install-format]: https://opam.ocaml.org/doc/Manual.html
[opam#2153]: https://github.com/ocaml/opam/issues/2153
[opam#45]: https://github.com/ocaml/opam/issues/45

## Implementation notes

### Layout key (digest derivation)

The `<digest>` component of `_build/install/<context>/.packages/<digest>/` is
the hex `Digest.repr` of the sorted explicit-package names and support-library
identities. A support-library identity contains both its owning package and
library name, so package-private names remain unambiguous across workspace
scopes. See `Install_layout.Key.encode` in
`src/dune_rules/install_layout.ml`. The sorting makes the digest
order-independent. A reverse table maps each digest back to its original
request so `gen_rules` can decode the layout dir's name when the engine asks
for rules. Hash collisions are detected at insertion time and raise a
`Code_error`.

### `For_rocq_only` escape hatch

`Install_layout.For_rocq_only.lib_root` exposes just the layout's `lib` root,
filtered to `Section.Lib` and `Section.Libexec` entries. Package-set layouts
follow opam's layout, where both sections install under `lib/<package>` and
`Libexec` additionally preserves the executable bit. Dune records native OCaml
plugins (`.cmxs`) in `Libexec`.

The special handling exists because rocq puts the layout's `lib` dir on
`OCAMLPATH`, where findlib walks eagerly; for a race-free, deterministic walk,
every entry findlib could see must be a declared dep. The bulk `env` function
isn't suitable here because it also pulls in root-section entries. For rocq
those include the theory's own `.vo` output
under `lib/coq/user-contrib/...`, creating a build cycle in the same-package
theory-plus-plugin case (the theory rule would depend on its own output via the
layout symlink). Filtering to `Section.Lib` and `Section.Libexec` excludes the
theory output while keeping METAs, `.cmi`, `.cmxs` etc., all of which are
upstream of theory compilation. See the comment on the module in
`install_layout.ml` for details.

## Future work

### Lock-dir build layouts

The layout mechanism can support the "in-and-out" problem ([#8652]): when a
lock-dir package depends on a workspace package, a layout containing the
workspace package's artifacts can be provided to the lock-dir package's build
env. The opam-like directory structure matches what build systems expect at an
install prefix.

Longer term, the layout overlay mechanism could replace isolated per-package
install directories for lock-dir builds entirely. Packages would build against
a layout that overlays all their dependencies' installed files into a single
virtual prefix, using dependency structure for overlay ordering.

### Incremental layout construction

If A depends on B depends on C, the layouts for their builds are {C}, {B,C},
and {A,B,C}. Instead of computing each independently, layouts can be built
incrementally: {B,C} hardlinks from {C} and adds B's entries, {A,B,C} hardlinks
from {B,C} and adds A's entries. This makes `_root` overrides natural: each
step replaces entries from the smaller layout. The key question is whether the
digest/key scheme can encode this incrementality or whether a structural
(DAG-based) key is needed.

Outstanding follow-up items are tracked in the Follow-up section of PR #14373.
