# Coordination

## Direction and order

Keep the mounting model; complete demand-driven loading and prove isolation
before pursuing estimated line savings. Pending semantics remain Ali's
decisions, not cleanup permissions. The measure of the stack is the
absolute size of `jj diff --from main --stat` (Ali, 2026-11-17): deliver
the decided functionality in the fewest changed lines, simplicity next.
S17 and S21 record remaining decision-gated reductions. S18 contains
the outstanding source-preparation correctness work and its prerequisites.

1. Settle the remaining environment and load-error contracts before
   changing their expectations.
2. Make S7 the principal completion gate, using S11's individually demanded
   package facts.
3. Move ordinary consumers through S10, retaining necessary dev-tool
   adapters. Then converge traversal and test structure on the stable model.
4. Record revision-specific submission evidence.

S13's layout cosmetics, B2's legacy-text restoration and per-file patch stage 3
are not independent work streams on this path. S5/S6 record constraints;
do not repeat their closed removal experiments without new evidence.

## The stack

`rpptuopt` "prototype: flatten package builds into a single build graph"
is one commit. Bookmarks `prototype/flatten-dune-v2` (the PR #16995
branch) and `prototype/flatten-dune-v3` are on it. It sits on five
commits for main, to be submitted separately and independently (none has
a changelog fragment yet):
- `tkkpyvkx`/`mkpovypq`: rules running a locked compiler's tools depend
  on its installation (`pkg/compiler-runtime-dependency.t`).
- `skpwyxsr`/`zqqqsqwt`: reject a package directory outside its project
  (`package-dir-outside-project.t`). PR note: main only rejects a
  directory outside the workspace.
- `nrwurxsr`: drop `pkg/bin-narrowing/duplicate-workspace.t`;
  `deps-bin-pform.t` covers it.
Ali rebases onto new `main`; recheck the actual base for
revision-specific validation and size comparisons.

The pre-squash history is on `origin` at
`prototype/flatten-dune-v3-history` (`4098c409`, base `b315a5a2`),
including the symmetric discovery spike (`d6dc0f3f`, also archived as
`spike/symmetric-library-discovery`). Do not fetch these refs into this
repo: their change IDs overlap the current stack. Use a separate clone
for historical evidence. Older history is at the local `stack-history`
bookmark. Owners must rebase parked workspaces before integrating.

`PLAN.md` lives in the prototype commit and in plan-only commits above
it. Do not squash plan edits into code commits. Remove the plan from the
submission diff.

Traps:
- Fetch targets are flat: `_build/_fetch/url-<digest>-dir` and
  `checksum-<sum>-dir` (#16945); use `Fetch_rules.make_target ~kind`
  (`Kind.t`, `File | Dir`).
- `Lock_dir.source_kind` returns a result. Choosing the local backing is
  deferred to source demand; a missing path must not fail metadata lookup.
- `pkg/patch.t` counts three links to a dependency's file where main
  counts two: the install layout exposes the file a second time in the
  sandbox.
- Formatting needs OCamlformat 0.30.0 (#17266).

## Swarm workflow

- Edit claims/decisions/progress in the main workspace's
  `/home/ali/dune3/PLAN.md`. Claim a bounded task in its heading; record
  workspace name/path and explicit base revision here.
- Code/tests go in separate jj workspaces under `/tmp`, never `~`.
  Copy existing `_boot/`; use private build dirs. Ask before bootstrap.
  Do not commit an outdated private copy of this plan.
- Dune runs freely in your own `/tmp` workspace (Ali, 2026-11-17). Only
  commands in the shared `/home/ali/dune3` workspace are serialized:
  reserve/release that slot here.
  In private workspaces use `env -u DUNE_SOURCE_ROOT ./dune.exe ...`
  so the check does not run in the shared tree. Before committing, run
  `build @check @fmt`; code/test changes also need affected focused
  tests. No full suite or expectation promotion without explicit approval.
- Tests precede fixes in separate logical commits.
- Integrate finished commits into the shared stack, preserving others'
  work: e.g. `jj rebase -r '<your-revset>' --insert-before 'dune3@'`.
  Verify with `jj log`/`jj status`, resolve conflicts and validate integration.
- After owned commands stop, `jj workspace forget <name>` from another
  workspace and remove only your temporary directory. Never delete others'
  workspaces/shared data or lock files to bypass contention.
- Report integrated hashes, checks and gaps; update this shared plan.
  Use `jj log PLAN.md` to find the local plan-only change, then
  `jj squash --into <plan-change> PLAN.md`. Do not squash plan edits
  into code commits.
- A rewrite of the stack leaves other workspaces stale. `jj workspace
  update-stale` discards edits made since the last snapshot: copy
  changed files (this plan included) aside first, then restore them.
- This file lists open work only; nothing here may repeat `jj log`.
  When a task is done, in the same plan commit delete your "Active"
  entry and the finished task or checkbox (and any audit item it came
  from). Leave no record of it. If a later task needs a decision, a trap
  or something deliberately left undone, put that one note beside the
  task that needs it. Commit ids, check logs, workspace paths and
  narrative belong in commit messages; find finished work with
  `jj log` (plan history: `jj log PLAN.md`).

## Claims

None. Claim a task as the workflow above describes before starting it.

## Pending: install cookie duplicate entries, fix for main (Ali)

Bookmark `install-cookie-once` is based on an earlier main, not the
stack.
The port covers cookie duplication on main; the stack's layout
collision is only reachable with mounts.
- [ ] Ali: push the bookmark and open the PR; then add
  `doc/changes/fixed/<PR>.md`.
- After that fix reaches the stack's base, drop the duplicate stack
  filter in `pkg_rules.ml` (`install-file-run` v2).

## Deferred: mount stage 3, per-file patch rules (needs Ali to reopen)

Keep whole-tree source preparation on the submission path. A prepared
directory is needed for modified sources, OCaml-syntax dune files and
rule plugins; see S8. A recipe still runs in a writable sandbox
copy of its mount, with leading patches already applied.

Per-file patching needs an origin manifest used by every derived backing
path and in-memory patching of dune files read during rule generation.
`Dune_patch.parse_patches` can supply the per-file operations, but does
not remove that structural cost. OCaml-syntax dune files still need a real
directory, so this would retain two preparation implementations.

Do not restore `Dune_patch.File_patch` or start the manifest work without
Ali reopening the design for a demonstrated need. Matching preparation
bodies and the source-input chain belong to S8, not a separate rewrite.

## Deferred: directory-symlink support (needs Ali)

Preserve main's local package behavior: file links are copied, while
source directory links are not traversed. Supporting directory links
requires approval; it is not a backing-compatibility fix.

Local backing uses `Path.Outside_build_dir.t` (workspace-relative or
external); fetched and prepared trees use `Path.Build.t`. Retain the
typed split and existing watcher keys instead of localizing aliases
inside `Fs_memo`. Keep logical mounts distinct from physical backing.

## Parked: no-narrowing spike, libraries (NOT for integration)

Workspace `dune-nosp`, `/tmp/dune-nosp.Mk7g/ws`, is on the old history.
Do not integrate it without its owner's rebase and a new review against the
accepted visibility policy below. Opaque eligibility, provider dependency
scopes and private-library boundaries must not be removed.

## Pending decisions (Ali)

Most important first.
- `Lib.L.project_root` is main's text again, including its `assert`;
  AGENTS.md asks for `Code_error.raise`. Keep main's text or not.
- C27 (S21) is blocked by S10's query migration, which needs Ali.
- Binary narrowing now covers the owning package's whole dependency
  closure (`Artifacts.package_deps`); main narrows to the packages listed
  directly. Expectations flipped false to true: `transitive-out` in
  `pkg/bin-narrowing/transitive-deps.t`, `r-avail` through `q` in
  `mixed-transitive-deps.t` and `workspace-transitive-deps.t`. Accept and
  record, or restore main's rule.
- `@ocaml-index` enters mounted packages:
  `pkg/ocaml-index-private-lib-gh10985.t` now fails in
  `.pkg/mypkg/.test_lib.objs`, so it no longer exercises the workspace's
  private library. Other workspace aliases stay out of mounts
  (`mounted-workspace-aliases.t`).
- Main's tests (about 1,500 diff lines): seven were rewritten into
  native/opaque matrices (~840, `bin-narrowing/multi-package.t` ~280),
  sections were appended (~620), and workspace-dependency fixtures were
  rewritten (~115, only the error-to-success flip is forced). Where the
  native cells and appended sections should live (main's files or new
  ones) decides how much of main's text comes back.
- Deferred (Ali, 2026-11-26): the "required by" chain of a demanded
  package that fails to fetch or load (`unused-locked-packages.t`,
  `unavailable-package-source.t`; also a load error reached through a
  consumer in DP and SP: `(libraries broken)`,
  a `%{pkg:..}` pform). S7 wants the chain. Not important yet. Notes for when it
  is taken up: the memo preparing a locked package
  (`Pkg_rules.find_locked`) has no description, which is why a fetch
  failure's chain stops at the fetch target; an error located in a file
  under `_build` gets no chain from the reporter at all, as on main.
  `source-with-directory-symlink.t` is the control that already prints
  a chain.
- Deferred (Ali, 2026-11-26): S17's `Package_mask` by-id precompute.
  Ignore for now.
- Deferred (Ali, 2026-11-24): the profile of a mounted package. It
  builds with the workspace context's profile, `dev` by default, where
  its recipe (`dune build -p`) would use `release`. There is no
  per-project profile loading and no requirement for one yet; decide
  when there is.
- Deferred (Ali, 2026-11-25), S16.F4: compiler-emitted `File "..."`
  paths are relative to `_build/<ctx>`, so for a mounted source they
  cannot be opened from the workspace root. Not specific to mounts: a
  generated workspace source has the same problem on main. Fix it, if
  at all, with a test on main, for generated
  sources in general on main; no mount-specific rewriting.

## Decisions in force (Ali)

- The compiler dependency (dev meeting, 2026-11-25) is on main under the
  stack (`mkpovypq`). Dependencies stay at package level; no PATH fix,
  and no other binary implies its package. Not covered: tools started
  indirectly (`(system ..)`, `ocamlfind`), and OCaml-syntax dune files.
  A sandboxed rule links the whole compiler installation; not measured.
- Accepted (Ali): for a mounted package a missing source is reported
  before its dependencies are built (`pkg/build-single-package.t`).
- Exported environment (Ali, 2026-11-25): the prototype follows main
  and claims nothing of its own. A package build replaces ambient
  variables with those of its dependencies and extends PATH only. The
  semantics are under discussion on main; take whatever main decides.
- Package cycles (Ali, 2026-11-25): not looked for when locking, as
  elsewhere in package solutions. A cycle through a workspace package
  is reported when building (`pkg/pin-stanza/cycle.t`).
- Toolchain compilers are to be removed (Ali, 2026-11-25): there is an
  upstream issue and PR removing the pkg toolchain mechanism. Do not
  spend work on behaviour that only they reach; the former S17.A3
  (`OPAM_SWITCH_PREFIX` for a toolchain compiler) is dropped for that
  reason.
- Two locked packages installing the same file (2026-11-24): an error
  for whatever depends on both, for now. Picking one, as main does,
  would make the order of `depends` matter (`pkg/installed-binary.t`).
- Action PATH (2026-11-24): a locked package is in an action's PATH only
  when the action depends on it (`(deps (package p))`). Declaring it in
  the owning package's `depends` does not add it, and `(system x)` does
  not find an undeclared provider that happens to be built. This is
  main's behaviour since #17023 (`pkg/bin-narrowing/`).
- Locked packages stay mounted in the workspace context (2026-11-24).
  A sibling context per workspace context was considered and dropped:
  the single context has the better behaviour. Do not re-propose.
- Minimisation choices (2026-11-17, Ali approved the dune3 agent's
  calls): main restored for V-P-E5 and P4; T5, D3 and the `load` move
  taken; N4 dropped. Kept as the stack has them, do not re-propose: P3
  (lazy per-package mld work), R8 (per-context memo), `bin/target.ml`
  `~contexts`, V-L-E5 (wasm reads `wasm_of_ocaml`'s version), U-E7, M9,
  A11 (main PR, not the stack), V5 (grows the diff).

- Conditional patches (B4; Ali, 2026-11-17): conditional evaluation alone
  must not switch a native package to opaque. Keep native support and
  evaluate conditions as early as their inputs permit, following the
  `enabled_if` precedent. Simplify variable expansion/dependency plumbing,
  not by dropping conditional support. Preserve preparation ordering and
  demand-driven loading; see S8.
- Path model: `dune_lang` names a directory by its place in a build
  context, a `Path.Local.t` relative to the context root; a mount is
  under the reserved `.pkg/<name>`. It never says where the files are.
  No `Path.Source.t` for a mounted directory, no `Source_path`.
  Build path: `Path.Build.append_local <ctx build dir> dir`; reverse:
  `Dune_project.dir_in_context`. Mounted?
  `Pkg_rules.package_of_artifact_path` (build path) or
  `package_of_dir_in_context`. Path for a message:
  `Pkg_rules.source_location` in the rules,
  `Dune_project.display_path` in `dune_lang` (the source tree supplies
  the display root). `Path.Source.of_local` on a project-model
  directory only where the project is known to be in the workspace
  (`bin/`, the upgrader, after a mount check).
- Errors about a mounted directory print its build path.
- `copy_files`: a mounted glob cannot leave its package;
  `(only_sources)` chooses the source tree over the build directory, in
  a mount as in the workspace.
- Library visibility (Ali, 2026-11-17): workspace and native mounted public
  libraries use ordinary Dune visibility for every requester. Opaque
  providers use self plus declared transitive dependencies; an unscoped
  requester keeps its unrestricted opaque candidate set. Private-library
  boundaries, provider-specific dependency scopes and opaque-parent self
  exclusion remain. Native discovery must not build an opaque recipe.
  Opaque fallback retains its reverse-workspace dependency checks. This
  does not change binary lookup or the `%{lib-available}` error decision.
  Known gaps: T21 records build-state-dependent shell lookup; under
  `-p a` a locked package depending on a masked workspace package may
  not be flagged; a `(package p)` stanza in an unscoped directory uses
  `p`'s scope for libraries and the directory's for binaries.
  The `(dir ..)` compiler-tool gap also exists on main and can be
  fixed there separately.
- Library order (Ali, 2026-11-14): workspace public libraries, the locked
  package named like the library (native or eligible opaque), compiler
  libraries, then the remaining eligible opaque scan. A miss can build
  every eligible opaque provider; keep discovery separate from permission
  to build. In a locked context the installed-library parent uses only
  the compiler-reported stdlib, not project OCAMLPATH imported through
  an absolute OCAMLFIND_CONF. Compiler priority for binaries is unchanged.
- Locked packages may depend on workspace packages (the `dune_pkg`
  changes, about 500 lines plus tests): stays bundled in this change.
  Dune's own lock needs it (`dune-configurator`, `dune-build-info`,
  `xdg` are required by locked packages). Do not split or re-audit.
- Single user context. Mounts at `_build/<ctx>/.pkg/<name>` are
  ordinary directories, never directory targets.
- Nothing under a mount is promoted, and rule generation says so; the
  engine knows nothing about mounts (Ali, 2026-11-13). Two parts:
  a promote-mode rule in a mounted directory is generated without
  promotion (`Super_context.make_rule`); and every action generated
  there is marked as not registering promotions. Mismatches still
  fail. Mounted cram rules are not generated.
  Rejected (Ali, 2026-11-26), do not re-propose: keeping `Promote` in a
  mount and having the engine skip target promotion when the action's
  `promotion` is off. It blurs the mode and the action flag. Known
  difference kept: a mounted `(promote (only ..))` rule hides a source
  that conflicts with an unpromoted target, which main reports.
- Intent of the action marker (Ali, 2026-11-13): the engine decides
  whether a diff may promote by guessing the source file from the build
  path and checking the disk, and nothing lets a rule opt out; the
  global `--disable-promotion` does not stop registration either. So
  `Action.Full` gets a per-action promotion property next to
  `corrections`, chosen by whoever generates the rule. With it off, the
  diffs that action runs (its `(diff)` actions and the sandbox's
  `.corrected` and patch-back diffs) still fail on a mismatch but
  register nothing. It is a general feature, not a mount hook: the
  value comes from the rule's own action, not from
  `Execution_parameters` or the directory.
- Binary lookup (Ali, 2026-11-13, amended 2026-11-25): the regular
  lookup, as on main. A directory owned by a package (a mount, or
  `(dir ..)`) searches that package's dependencies, an action's
  `(deps (package ..))` and the compiler's packages, and builds no other
  opaque package. A directory with no owning package searches the whole
  lock directory, as on main: every locked package is then built
  (`pkg/cram-path-into-build-dir.t`). So a locked package that fails to
  build natively breaks any build that does such a lookup; dune's own
  `doc/dev/releases/dune` does one, which is why its lock needs the
  Menhir patch (S14). The rule lives in `Pkg_rules.lock_dir_binaries`.
  The context-wide C compiler probe (`Cc_flags`) has no directory and
  searches the compiler's packages only: searching everything there is
  a dependency cycle once an opaque package depends on a native one
  with C stubs (`pkg/mounted-source-lookups.t`).
  Possible later refinement (Ali): scope by the stanza's package as
  `-p` attributes it, as library lookup does, instead of by directory.
  `Super_context.resolve_command_line_program` still adds every locked
  package for `dune exec prog` from an owned directory.
- The workspace install directory `_build/install/<ctx>` holds workspace
  packages only (Ali, 2026-11-24; replaces the 2026-11-13 decision).
  A locked package has no install file or install alias. Its installed
  files are reached through a targeted layout (`Install_layout`), which
  is what `(package p)`, `%{lib:p:file}` and old-style binary lookup
  use. Contributing locked packages to the workspace install directory
  is a possible later addition, not a requirement.
- Raise load errors on demand, not through catch/store machinery (S7).
- Every locked package with a source is mounted. Modified sources use
  one sandboxed rule outside the mount and per-file copies into it. A
  recipe runs in a sandbox copy of its mount, without the leading
  patches preparation already applied. A recipe that transforms its
  source further (a substitution, a later patch) keeps the package off
  dune rules until S7. Otherwise a dune file belonging to a project that
  declares the package selects dune rules (parked principle, Ali,
  2026-11-12: the build instructions say how the source is prepared;
  after preparation the project decides).
- Workspace/lockdir checks run at rule generation, not `Lock_dir.get`.
- Keep mount-required Shared_locks (private locks without a default
  context), jsoo Config/runtime keys, cinaps/mdx mounted listing,
  the workspace load in `Dune_load` below `Pkg_rules` (partitions in
  `Partitions`); do not re-audit.
  jsoo's digest-keyed global table is a later redesign candidate.
- `action_project_root` is a `Path.Local.t` (the engine appends it to the
  context root).
  Sandbox dependency linking is back to main; reproduce renewed
  directory-overlap failures before changing it.
- No `doc/` changes in the prototype itself, except its changelog fragment
  (`doc/changes/added/16995.md`). The `dune-project` `depends` move stays
  separate. Compile-commands: see D12.a.

# Open work

S7/S11 are the critical path; S10 moves their remaining ordinary
consumers. Follow the ordering above rather than the old size estimates.

Review goal (#16995): no hidden stanza or engine change without a
demonstrated need; promotion permission belongs to rules. Preserve
behaviour unless Ali explicitly agrees otherwise.

## Engine

### S5. Necessary engine support (retained constraints)

Keep the build-only root (`With_sources { build_only_sub_dirs }`,
`is_build_only_root`, parent-rule handling and `Unrestricted`):
loading the locked compiler must not first load workspace root rules
that need that compiler to evaluate their OCaml-syntax dune file.

Keep `Build_config.Gen_rules.Rules.sources` (file copy rules and source
subdirectories, one optional inventory for the directory) and `directory_target_contents_opt`.
Mounted files are ordinary rules but still need source-versus-rule
arbitration for fallback/promote rules and directory targets. Folding
source directories into build-only directories would reject valid directory
targets over mounted source directories.

Do not repeat the inventory-removal or `print_diff.ml` E4 experiments
without a new requirement. Separate/empty contexts remain out of scope.

### S6. Source-copy compatibility with ocaml/dune#16747

This is a rebase watchpoint, not a new removal investigation. #16747's
engine copy table holds workspace paths; mounted files remain ordinary
rules and still need S5's inventory. Preserve that distinction when
integrating upstream source-copy changes.

Do not move arbitration back to the rules side: the earlier spike made
subdirectory `copy_files` cyclic and lost duplicate-source arbitration
(`pin-stanza/duplicate-pins.t`). Existing workspace copy handling and
mounted supplied rules must retain equivalent arbitration.

### S7. Demand-driven loading (principal completion gate)

Only demanded packages are fetched/loaded; ordinary errors preserve the
"required by" chain. Do not catch/store them. Packages that build with
`dune build -p <pkg>` under opam must load here, subject to S14's accepted
vendored-public-library limitation.

Design (Ali: "seems fine, go for it", 2026-11-12): a package's kind
(native or opaque) needs its fetched source, so nothing context-wide
may ask it of every package. Landed:
- Env from lock metadata: PATH/OCAMLPATH etc. list every visible locked
  package's prefix without asking its kind; a native package's prefix
  is never populated.
- `Partitions.workspace` loads the workspace per context;
  `Partitions.mount` loads one locked package on demand. Scope and
  binary discovery use those partitions (`Scope.DB`, `Artifacts_db`).
  Preserve library eligibility and lookup order from "Decisions in
  force" (`pkg/library-lookup-order.t`).

A package that fails to load raises where it is asked for (Ali,
2026-11-24); nothing catches or stores the error. Looking a library up
consults the package named like it, then scans the eligible opaque
packages; a native package is never scanned for another name.

Use S11's existing metadata-only candidate and adjacency indexes for the
remaining consumers. Keep source-dependent facts individually demanded.
Avoid duplicate loader/scope tables, repeated closures and memos over memos
as part of this migration, not as competing cleanup tasks.

Traps for consumers of the lazy database: a local library id carries
its local name, so look an id up by its directory's partition, never by
name. dune's own bootstrap rule reads mounted packages as installed
findlib packages through OCAMLPATH (`boot/dune`, nested dune), so the
generated dune-package/META of a mount must be complete.

Work in order, coordinated with S10/S11; `pkg/unused-locked-packages.t`
records the current behaviour and flips as the steps land:
- Not work for this stack (Ali, 2026-11-26): a native package's
  exported env (`setenv`) is applied to the context env with paths
  under its unused prefix. The exported environment is being considered
  separately on main.

Observable exit criteria:
- Unrelated workspace targets do not contact unused HTTP/Git sources or
  fail on an unused broken package (`unused-locked-packages.t`,
  `git-source.t`, `source-with-directory-symlink.t`).
- A named native-library or compiler-library hit does not load unrelated
  mounts. Install-directory lookups do not enumerate every mount.
- Demanded failures through package deps, libraries and package pforms
  retain their file location (the "required by" chain: Pending decisions).
- Deprecated library names, declared binary visibility, host/target
  selection and dev-tool sharing/isolation still work.

Profile null builds on the submission revision if claiming a performance
benefit. Record package loads, cookie reads and dune's own time separately
from processes; pre-migration trace counts are not current measurements.

## Rules

### S8. Follow-ups to source preparation

- [ ] Simplify early conditional-patch evaluation following `enabled_if`.
  Source preparation and recipes already share `Slang_expand.eval_blang`;
  investigate common variable resolution and dependency handling, not a
  second boolean engine. Preserve metadata-only conditions without
  building dependencies, false-condition skipping of missing patch files,
  preparation order and lazy loading. Conditionality alone is not a
  reason to select the opaque backend (B4). Concrete correctness work is
  S18.A2; keep the detailed tasks there rather than duplicate them.
- Mounted substitution waits for S7; otherwise eager loading would make
  source preparation build dependencies on every cold build.
- OCaml-syntax dune files and mounted rule plugins still require a real
  prepared directory; preserve private writable execution and origin
  immutability. Per-file patch stage 3 remains deferred.
- Whole-package re-preparation after one changed file is an accepted cost;
  note it in the PR. No rebuild-granularity comparison test exists.
- Known limitation, not pursued (Ali, 2026-11-12): OCaml-syntax dune
  files of mounted packages are re-run on every dune invocation, as
  workspace ones are on main, once per mount (twice for two native
  packages sharing a source), and for every such file of the source.
  Caching them as build rules was judged too complicated here. They run
  in the package's private copy (`_private/<ctx>/.dune-source/<name>/
  source`), never in `_fetch`; an unmodified package gets that copy only
  if it has such a file. A script that writes into its directory dirties
  that rule output, so the copy is redone on the next build; one that
  writes nothing leaves it alone. Still true: a `git` command in the
  script sees the user's repository, because `_build` is inside it.

### S10. One opaque-build path instead of two

Dev tools build through the same recipe rule as mounted packages; the
legacy executor is gone. `Pkg` and `Resolve` remain as a facts layer
reading candidates; the queries still use them. Accepted leftovers: dev
tools read dependencies from their own prefixes, not through the install
layout; a cycle in a dev-tool lock is reported by Memo's "- package"
frames, not `DB`'s message with versions (the solver does not produce
one); `pkg/mounted-source-preparation.t` lost its "Package digest index"
log line. The binaries map is built twice.

Seam (Ali, 2026-11-17): no package digests or legacy executor at submission.
Move consumers off the `Pkg_rules` adapters; keep `Dune_pkg.Package_universe`
for lock/formula semantics.
- Each dev tool is its own package set, mounted at
  `_private/default/.dev-tool/<tool>/<name>`. Its packages stay opaque
  recipes: native builds would need the private context threaded through
  source mounting, so they are a later step if wanted.
- One compiler everywhere. With a project lock dir, every tool is solved
  against the project's compiler (`bin/lock_dev_tool.ml` does this today
  only for `needs_to_build_with_same_compiler_as_project`), and the
  compiler package (`lockfile.ocaml`) with its closure resolves to the
  project's `.pkg/<name>` mounts, checked equal to the tool's lock entry.
  Nothing else is shared, with the project or between tools.
- Without a project lock dir, tools are solved against the system
  compiler (a system-compiler constraint, one platform); that package is
  cheap, so each tool's set holds its own.

The base has already removed `print-digest` and uses name-keyed,
isolated universes, with no compiler sharing. The prototype uses
canonical mount paths for ordinary lock candidates and package prefixes.
Remaining work:
- [ ] Compiler sharing: widen the lock-time compiler rule above. Test
  compiler-closure sharing with a project lock, isolation of all other
  dependencies (including identical lock entries), and system-compiler
  operation without a project lock. Retain host/target coverage.
- [ ] Needs Ali: move the queries (package dep, cookie/installed-file,
  toolchain, binary, env, OCAMLPATH, depexts) off `Pkg`/`Resolve` to
  per-context package facts. Judged churn without deletion: the roughly
  300-line layer would be rewritten at about the same size.
- Known, minor: `gen_mounted_rule` relocates the build command
  (`relocate_build`) but executes `Prepared.rest`, computed before
  relocation, so a prepared source's residual recipe keeps unrelocated
  locations (error messages only).

Keep the common executor in Pkg_rules and retain Dune_pkg.Package_universe
(formulas, visibility, name checks, freshness). Validate opaque installs,
scoped env/binaries, toolchains and dev-tool formatting/utop/sharing.
Resolve/executor wrappers, repeated variables/dependency/binary folds,
and repeated install-entry/cookie loading belong to this migration.
Verify/cache installed facts per context/package, not per consumer.
Do not grow the diff by rewriting main's legacy source scanner merely
to reuse a new helper. Recipes still depend on the mount's files, not
only its backing directory; preserve writable-copy semantics.

### S11. One package graph per context

Supply the facts needed by S7 first, then migrate S10's ordinary callers.
`Pkg_rules.candidates` and `Pkg_rules.dependencies` already provide
name-keyed, per-context lock metadata without fetching sources. Reuse them
where appropriate; demand source-dependent provider, project and installed
facts individually per context/package.

Preserve dependency semantics, not just similar-looking code:
`Partitions.package_closure_for_context` follows authored workspace
dependency names; `Package_layout.materialize_dependencies` resolves
workspace formulas against candidate versions and the solver environment.
Check alternatives, optional/platform dependencies and workspace cycles
before sharing work. Retain dev-tool isolation except for S10's compiler
closure sharing.

Reuse shared locked adjacency; keep workspace formula resolution distinct
from the authored-name closure.

Exit: constructing the index touches no package source, independent
consumers reuse the demanded facts, and neither closure nor version lookup
reintroduces a context-wide classification pass.

### S13. Build-directory layout (deferred unless needed by S10)

Keep `_build/<ctx>/.pkg/<name>` for project mounts and
`_build/_private/default/.dev-tool/<tool>/<pkg>` for dev tools. Prepared
sources stay in `_private/<ctx>/.dune-source`. Revisit path plumbing
only when S10 removes the legacy adapters.

### S14. Mounting model (decided) and the Menhir patch

Decided (Ali, 2026-11-13): one mount per locked package, masked to that
package. A source that vendors a project with its own package is
mounted like any other; nothing special is done for vendored packages,
so a vendored *public* library is masked out and not found
(`pkg/mounted-projects.t`). The native-or-recipe condition has no
vendored case. Vendored code without packages (private libraries, the
common case) loads natively.

Menhir releases with vendored public libraries need a patch or a
different package split when built natively. The older lock showed
`Library "vendored_pprint" not found`; check the source selected by the
current solver rather than assuming this version-specific failure.
- The patch makes Menhir's copies of `fix`, `bitsets` and `pprint`
  private libraries of Menhir's own project and drops the `vendored_`
  prefix and its alias modules; the directories stay `vendored_dirs`.
  In `~/repos/menhir` (local only, nothing pushed): bookmark
  `unvendor-master` on upstream master, `unvendor-20260209` on the
  release commit. Lock patch:
  `git diff --no-renames 20260209 unvendor-20260209`, applied as the
  first step of the `menhir` package's build action (only that one of
  the five Menhir packages needs it).
- Not work for this stack (Ali, 2026-11-26): the package's build is
  broken, not dune. The patches will be shipped to Menhir eventually;
  until then a lock needs the patch applied (`fixup-lock.sh` in Ali's
  benchmark checkout).
- Note in the PR: a locked package whose source vendors a public
  library in its own package fails to build until it is patched.

Considered and not pursued: sharing vendored projects between the
mounts of one source (a shared backing is available, but this needs a
representative, a visibility exception and a rule for patched copies);
one mount per source (experiment `zsuxuymo`:
Menhir built once natively, but a directory would no longer have one
owning package); vendored packages private to their mount
(experiment bookmark `exp-vendored-native`: net
zero lines, passes `pkg/` on a fixture, not reviewed).
In dune's lock only three sources are shared by several packages
(Menhir 5, js_of_ocaml 2, odoc 2).
Upstream context: #4070 (why Menhir renamed its vendored copies), #7058
(package subsetting for vendored directories, unimplemented), #3911,
#6830.

## Diff hygiene

### S15. Remove unrelated changes from the diff

Compile-commands: D12.a. Other listed items are resolved; do not repeat
audits. For new suspects, inspect
`jj diff --from main --stat`: state a mount-related need or revert.
For main, separately: `Lib.DB.with_parent` keeps `instantiate` closed
over the parentless database, so libraries it instantiates resolve
deps without the parent scope (the stack restored main's text in
`sovrlpqv`; only the utop dev-tool lock dir reaches it, untested).

### S16. Generalise workspace source paths in the rules (partly done)

Direction (Ali): wherever the rules rely on a `Path.Source.t`, they must
work with sensible semantics for mounted sources (a build path, or a
`Path.Local.t` place in the context; see the path model under
"Decisions in force"). A
mounted directory's source is its own build directory; stripping the
context to get "the source" yields a workspace path that does not exist,
so lookups silently answer "nothing" or answer for the workspace root.

Invariants:
1. Rules never compute a source path from a build path by removing a
   prefix themselves. A build directory's place in its context is
   `Dune_project.dir_in_context dir`, its display path
   `Pkg_rules.source_location dir`, and its loaded directory
   `Partitions.source_tree_dir ~dir`.
2. One directory type serves both kinds (`Source_tree.Dir.t`).
3. Looking upwards never leaves the mount.
4. `Path.Source.t` in the rules means the user's workspace: promotion,
   VCS, cram, lock directories, `bin/`.
5. No new hook in `src/source`; lookups are keyed by build directory.

Decided semantics for mounted sources (Ali, 2026-11-12):
- Listing, dune file, project, status: from the mounted tree.
- Warnings: off, as for vendored. Formatting and cram: no rules.
- Promotion: never; a package that needs it (melange `(promote ..)`) is a
  user error for now.
- Install destination beginning with `..`: fatal.
- Merlin and ocaml-index: generated too, with the mount as source dir;
  the `@ocaml-index` alias and merlin aggregate the partition's indexes.
- Locations and line directives supplied by dune use the full build path,
  `_build/<ctx>/.pkg/<n>/d/x.ml`. Compiler-emitted `File "..."` text
  keeps the compiler's short path (S16.F4, deferred above).
- Package version in build-info and `%{version:..}`, VCS: parked; leave
  as today.
- odoc: a mounted package's docs build through its mount, minimal
  support only; no further odoc features or tests for mounts.

Remaining work, one feature per commit, test first:
- Leave `format_rules.gen_format_alias`'s workspace lookup alone while
  mounts get no formatting rules. `warning_emit`'s
  `Source_dir_or_enable of Path.Source.t option` is workspace-only:
  mounted escapes are errors.
- Deferred (Ali, 2026-11-25): `odoc_new`'s `is_vendored` query covers
  workspace projects only. Whether mounted projects take part is
  deferred: odoc is being rewritten, so nothing meaningful can be
  decided until that lands.
- Parked: mounted `doc-new` mld/index parity. The test/fix pair is in
  `dune-doc-new`, `/tmp/dune-doc-new.bKsxNX/ws`, based on `snqzlotl`
  and not integrated. The workspace owner should validate the pair on
  the chosen tip before integration. It does not change vendored status.

Leave alone:
- `Path.Source.t` used as a context-relative name: `top_module`,
  `bootstrap_info`, `melange.ml:57`, `super_context:23`, `scope:775`,
  `module.ml:34`, `install_layout:66`, `install_rules:1184`,
  `context.ml`, `pkg_rules`, and `extract_build_context` uses that only
  read the context name.
- Workspace-only by nature: `cram/`, `lock_rules`, `lock_dir`,
  `dune_load`, `loaded_project` identity, `pkg_rules`' lock candidates
  and source preparation,
  `bin/`, the upgrader, `pin.ml`, `Dune_project.gen_load`.
- Parked with versions: `link_time_code_gen`, `artifact_substitution`,
  `expander` `Git_sha`, `site_env`.
- Prefix-stripping calls read on 2026-11-16 and found context-relative
  or mount-aware, do not re-audit: `merlin.ml` dump-config `source_path`
  is `<context>/<local>`; `source_deps`, `action_unexpanded`,
  `simple_rules` copy_files, `rule_plugin_runner`, `file_binding_expand`
  and `melange_rules` branch on the mount first; `foreign_rules` only
  tells external directories apart; `env_stanza_db`, `library.ml`,
  `alias_rec`, `dune_load`, `dir_status`, `cram_rules` and
  `install_rules` symlinks only read the context name.

Mounted cinaps, mdx and melange have tests (`pkg/mounted-{cinaps,mdx,
melange}.t`). Named gap: their runtime libraries (`cinaps.runtime`,
`mdx.top`, `melange`) are not found from a mount; the cinaps and mdx
tests record that error, and all three record only the rule paths before
it (listing, `(package ..)` mask, emission with `(emit_stdlib false)`). A mounted `melange.emit`
output repeats the mount path (`out/.pkg/foo/main.js`), as a workspace
subdirectory does. Utop has no established mounted CLI entry point.

Done when the remaining workspace lookups are intentional and every
generalised feature has mounted coverage or a named gap. Audit
`Source_tree` lookups and `drop_build_context` when moving consumers;
do not use the old grep snapshot as the current inventory.

#### Path model follow-ups (unclaimed unless marked)

- Ali to review the current path-model diff against main (S16), not an
  archived subchange. Final-tip checks are under "Validation and submission".
- Factor the type change (`Path.Source.t` to `Path.Local.t` in
  `dune_lang`, `action_project_root`) into a patch on main (Ali: later).
  Rule-side ids (`Lib_id.src_dir`, `Dune_file.dir`) as `Path.Build.t`
  belong to that patch, not here: main has the same conversion line at
  each of the ~9 sites.
- Names that now mislead, undecided: `Source_tree.Dir.source_path`,
  `Source_tree_file.File.source_path`, `Loaded_project.source_root`,
  `Expander.source_dir` return the place in the context.

### S17. Minimisation candidates (audit 2026-11-17, unclaimed unless marked)

Four read-only reviewers read the whole `src/` diff against main.
Every item below was verified in the current code, not from hunk
headers. Savings are changed lines against main. Restore main's text
where the item says so, keep each change its own commit and validate
with the named tests; no full suite. Remaining items need their named
decision, the owning migration or a fresh audit against the accepted
contracts; old estimates are not permission to remove required plumbing.
Labels are local to S17: cite them as `S17.B3`. A bare `A6`, `A9`,
`A10`, `A11`, `B1`, `B2`, `B4`, `C2`, `D12` or `D13` anywhere else in
this file is the older audit note, deferred reduction or test item.

Packages (`pkg_rules.ml`, `package_layout.ml`):
No test reads the `Log.info` lines "Package source candidates" and
"Prepare package sources" any more; they can go.
Replacing `Pkg.dep` with installed-file facts can
change opaque dependency coverage; migrate it in S10, not as a cleanup.
Keep the `install-file-run` v2 filter until the cookie fix reaches the base.

Library and binary resolution (`scope.ml`, `lib.ml`, `artifacts*.ml`):
Re-audit any proposed removal of `Scope.DB.for_package`,
`Expander.for_package`, the partition's exclusive-package branch,
`by_package`, `create_db_from_stanzas`, `Found_or_redirect.owner`,
`Scope.public_libs`, `scope_host` and the ppx `opaque_packages` map
against the accepted visibility policy. Native-public symmetry does not
remove their opaque/provider/private-scope duties; old line estimates are
not removal permissions. Not recommended: undoing the `analyze_binary`
helper extraction in `artifacts.ml` (duplicates 14 lines or changes
local-before-native precedence).

Loading, project model, source tree:
Deferred (Ali, 2026-11-26): `Package_mask` by-id precompute (~20);
it conflicts with the build-path message decision.

### Kept on purpose (do not re-audit)

Read and found necessary, or measured and not worth it. Re-open only with
new evidence.

Source, found necessary: `build_env_of_deps` is main's text (A9: sharing
it is +11); jsoo `Config.compiler`'s "different backend" error (D8: a
dynamic dep on the shared runtime path reaches it);
`lib_entries_of_package`'s empty-set match (B12: removing it re-indents
28 lines); the two prepared-dir computations (C4: a shared helper is +6);
`Lib.is_local` via `Obj_dir.to_local` (B5: main's text calls opaque
installs under `_build/default/.pkg` local, 17 `pkg/` tests fail);
`owner_db ~include_owner:false` (B3: with the package included, a hidden
library lookup re-enters its own database forever,
`pkg/findlib-meta-optional-directory.t` hangs); the S5 engine inventory
and build-only root; the action-marker promotion property through
`print_diff`'s option; `warning_emit`'s context bag (net -32); the
install-entry `~source_tree_root` (mount-confined globs); `merlin`/
`ocaml_index` `~owner` (F8); `Backing`'s path-derived equality,
`File.parent_exn`/`relative`, `Include_stanza` mount-root checks;
`Dune_load`'s reserved `.pkg` check; `Loaded_project`'s masks;
`Dune_project` `display_root`/`name_hum`; `Source_file_dir.rules`;
`lib.ml`'s `src_dir` comparison (A10); `Findlib.create_for_package`;
`resolve_command_line_program`; `Install.Entry.Expanded.make`;
`Paths.of_root`; `Layout.materialize`; `layout_env`;
`workspace_dependencies`; `compiler_packages`; `Copy_source`; the recipe
executor seam (S10); the `dune_pkg` bundle and the `Path.Local` type
change (parked); `Package_name.file`; the logical/backing pair in
`Source_tree_file`; `bin/alias.ml` and `bin/target.ml`;
`Dependency.export_paths`; native branches in
`build_env_of_deps` and `Pkg.dependencies` (a dev tool can share a
natively built dependency); the `install_paths` override; the
per-package `package_entries` memo;
`Package_universe.immediate_dependencies`; `Normal.combine_exn` map
unions; `simple_rules` `only_sources` branch; the one-line build-path
adaptations in `cram_rules`, `bootstrap_info`, `revdep_rules`,
`ocaml_index`, odoc; `directory_target_contents_opt` (three callers).

Closed, do not revert or retry:
- L3: `ml_sources.ml` threads `~source_dir` for message text; reverting
  would print `.pkg/n/d` (S16 chose the full build path).
- L8: the dune-project existence check stays guarded inside its body.
- L9: `Loaded_project.source_tree_dir` and `Partitions.source_tree_dir`
  are not duplicates.
- P10: restoring the dead `Lock_dir.transitive_dependency_closure` and
  `Packages.choose_pkg_for_platform` at main's positions.
- F8: per-package scopes cost about 60 lines in the feature rules
  (`Merlin.make ~scope ~expander`, `library_rules ~expander`,
  `for_package` in `exe_rules`, `lib_rules`, `melange_rules`); they exist
  because a stanza's scope can differ from its directory's. Preserve the
  opaque/provider scope duties (A9, B1).
- C7 is not behaviour-preserving (a broken-symlink `dune-project` or
  `*.opam` in a subdirectory makes the shared `gen_load` reader raise
  `Option.value_exn`); C11 and C15 save 2 lines each; C12 (+9), C16 (+9,
  `files` is main's code), C17 (+1..+3) and C14's `package_prefixes`
  guard (+2) grow the diff; C14's Install_action bullet would revert
  `uvymruxx` (-7); C6 nets +1 unless `Preparation` is reshaped.
- B5 (S22) realigns the diff for 0; C1 (S22) nets -4 (a per-candidate
  table helper costs ~16 lines); D5 (`Rules.create ?sources`, -7) adds an
  optional argument (AGENTS.md).
- Reviewers' rejections: moving relocate helpers back into `Resolve`;
  the dead `Dependencies` branch of `Paths.make` (grows the diff);
  inlining `execute` (executor seam); `install_rules.ml` `~dir` of the
  dune-package/META/odoc-config rules (needs a test run to judge); the
  `Path.Local` and `Dune_load`->`Partitions` call-site churn (needed).

Traps:
- Restoring base's leading `let* () = Memo.return ()` in
  `Source_deps.files_with_filter` makes `watching/batch-mode.t` and
  `watch-mode.t` rerun a failing action (counter 2, not 1). Why Memo
  ordering changes the caching of a failed action is not investigated.
- The ppx key stores a mount's directory as a `Path.Source.t`, against
  the path-model decision that a mount never gets one (not a saving).

Boundaries the simplifications must retain:
- `Source_tree_file.Backing/File/Dir`: logical identity, physical reads and
  confinement are distinct concerns. Narrow construction, not the model.
- `Source_tree` versus `Source_file_dir`: authored-tree interpretation and
  source-copy rules/engine arbitration have different jobs.
- `Dune_load` (workspace load, main's module) below `Pkg_rules`, and
  `Partitions` above it: context-independent workspace discovery avoids
  package-loading cycles; do not fold partitions back into `Dune_load`.
- `Candidate`, `Preparation`, `Mounted`: metadata, required preparation
  and source-dependent classification are distinct stages. Share indexes
  rather than collapse them into an eager record.
- `Loaded_project` and `Partition`: ownership/masks and distinct project
  versus partition roots matter; partitions are lazy loading units.
- `Package_layout`, `Pkg_rules.Layout`, `Install_layout`: provider
  selection, dependency materialization and installation rules remain
  distinct responsibilities even when they share an entry calculation.
- `Pkg_rules.Dependency/Paths/Pkg`: installed facts, path handling and
  legacy executor/dev-tool compatibility are not automatically redundant;
  migrate ordinary consumers through S10 before deleting adapters.
- `Dune_pkg.Package_universe`: formula resolution and installation-universe
  isolation are not ordinary visibility closure.
- `Scope`/`Artifacts`: preserve opaque forcing eligibility, provider/private
  scopes, compiler precedence, reverse-workspace guards, duplicate
  diagnostics and current error semantics. Accepted native-public symmetry
  is the baseline, not a new unclaimed native-filtering cleanup.
- Preserve engine source inventories, build-only roots, directory-target
  arbitration, whole-tree private preparation and per-action promotion
  permission. Per-file patching and directory-link traversal stay deferred.
- Environment/PATH, mask and documentation policy choices are not
  mechanical simplifications.

### S18. Abstraction audit follow-ups (Astra; unclaimed)

The remaining correctness tasks are supported by static reading of the
current source; neither has an executed reproduction recorded here.
Re-read the implementation and callers before claiming an item. These
are not deletion estimates or permission to change policy.

Keep one bounded change per task/workspace. Bug regressions must precede
their fixes in separate commits. Preserve all existing observations;
focused validation is proposed below, not recorded as already passing.

#### Correctness first


- [ ] S18.A2: propagate unresolved condition inputs before Boolean coercion.
  Classification: suspected bug/new S8 evidence; high static confidence;
  medium scope because dependency staging must be designed.
  Anchors: `Pkg_rules.eval_source_condition` and
  `Slang_expand.eval_blang`.
  Early expansion substitutes `""` for an unavailable value and checks an
  `unsupported` flag only after evaluation. A direct Boolean input such
  as `%{pkg:dep:feature}` can therefore fail coercion before that flag can
  defer evaluation. Unknown and false are different states.
  Keep unavailable inputs explicit, distinguish them from genuinely
  undefined variables, and resolve required dependencies before Boolean
  conversion. Reuse the shared evaluator and follow the `enabled_if`
  staging precedent; do not select opaque merely to avoid evaluation.
  First reproduce with a Boolean supplied by a built dependency. Retain
  the metadata-version condition that does not build its dependency and
  the false condition that never reads its missing patch file in
  `pkg/mounted-source-preparation.t`. Preserve demand-driven loading and
  preparation order; coordinate with S8/S7 for source-dependent inputs.

#### Structural work: coordinate with the owning migration

- [ ] S18.A8: assess explicitly context-bound file evaluation with S7.
  Classification: loading interface; high confidence; no established
  changed-line saving. Anchors: `Dune_file.eval` and its caller
  `Partitions.Partition.make`.
  The evaluator returns a context-taking function whose sole caller
  immediately supplies an already-bound context. A direct context argument
  could remove that stage, but this factory is inherited from main.
  Compare the actual-base diff before changing it: removing a few net
  file lines is not enough if unchanged main text must be reindented.
  Revisit alongside necessary S7 evaluation work, not as an independent
  factory/owner-wrapper rewrite. Preserve validation, host selection,
  initialization timing, plugin registration and lazy dynamic stanzas.
  Check mounted OCaml-syntax cases and cross-context rule plugins,
  including `stanzas/rule-plugins/mounted-source-preparation.t`.

- [ ] S18.A9: consider explicit absence of a parsed opaque partition.
  Classification: duplicate absence representation; medium confidence;
  small scope only after error/fallback prerequisites are established.
  Anchors: `Partitions.load_mount` / `partition_of_dir` and
  `Scope.DB`'s `of_dir`.
  Opaque packages receive empty parsed partitions, requiring empty-map
  handling in loading and scope lookup. Consider `None` for no parsed
  Dune partition instead of constructing empty facts.
  Do not remove the physical mount or its ordinary source-copy rules.
  S7 must first establish equivalent error propagation and safe workspace
  fallback under reserved `.pkg`; re-check the scope side after the
  accepted library-discovery integration.
  Check `pkg/mounted-eligibility.t`, `pkg/mounted-opam-sources.t`,
  `pkg/extra-source-overlap-with-source.t` and reserved-`.pkg` controls. Treat this as
  an investigation, not an approved removal.

Next independent correctness task: S18.A2, through S8; A8 and A9 wait for their S7 prerequisites.
All test paths above are relative to `test/blackbox-tests/test-cases/`
unless they begin with `test/`. Re-estimate savings after validation;
do not add overlapping estimates or promise a large deletion budget.

## Tests

S19/S20 organise tests after their feature contracts stabilise. Old audit
line numbers are navigation hints, not deletion instructions: re-read the
current fixture and source.
Keep one logical change per checkbox, name surviving coverage for any
deletion and ask Ali before promoting. Policy-dependent
expectations wait for the named decision.

### `pkg/` tests that record wrong behaviour

Each has a note in the test; fix and flip:
- Deferred (see "Pending decisions"): demanded source/load errors lose
  "required by" in `unused-locked-packages.t` and
  `unavailable-package-source.t`.
- `sites-plugin.t`: not to be fixed (Ali, 2026-11-26). dune-site does
  not work with package management at the moment; the test records the
  gap.
- S14 (Ali, accepted): `mounted-projects.t`, a source vendoring a
  public library in its own package fails natively with "Library .. not
  found". Fixed in the package (Menhir patch), not in dune.

### S19. Deduplicate the added tests (remaining items unclaimed)

Line numbers in the second test audit for DP, RP, SP, OS and PM are
stale.

Rule: deleting a case must not delete the only assertion of a behaviour.
For each deletion name the surviving case in the commit message. The
remaining expected output of a file must not change.

- Kept on purpose, do not redo:
  - RP's promoted-directory-target and `--promote-install-files` cases
    assert more than writeback; their writeback checks are on the
    workspace (`test ! -e .pkg`).
  - PM's `test ! -e .pkg` lines: they follow `--auto-promote`,
    patch-back or `dune promote <file>`, which can write without
    registering, so an empty `dune promotion list` does not cover them.
  - `Source.Dune_file.logical_path` in `dune_rules/dune_file.ml`: the
    basename can be `dune-file`, so `Source.Dune_file.fname` is not a
    substitute.
  - `@pkg-install` META in DP and PR: not duplicates (version vs
    `requires`).
  - RP warning policy, `no_keep_locs`, duplicate-dependency: the
    workspace and vendored runs are the control; without them a silent
    mounted build shows nothing.
  - RP relative globs: the second test audit's mapping to
    `install-glob-relative.t` is wrong (that test is about the `..`
    destination error).
  - DP's opam/odoc case compares against the workspace output with
    `cmp`; the control stays (Ali, 2026-11-26).
- [ ] D12.a after #16977 merges/rebase: remove the superseded
  compile-command source/test changes together (`compile_commands.ml`,
  ~241 lines; `lifecycle.t`, `copy-files.t` and CC's extra
  cases). With it, C31: `Loaded_project` stores `build_dir` (also used by
  `output_path`); its only other use is the compile-commands rule in
  `gen_rules.ml` that compares `dir` with it; drop that use if #16977
  replaces the rule. Do not drop
  mount-specific coverage that still protects a retained source change.
  `copy-files.t` no longer tests that `dune build compile_commands.json`
  is (not) a target.
- [ ] D12.b audit source/test pairs before removing unrelated changes:
  `gen_rules_combine_tests.ml`, `library-enabled_if-with-package.t` and
  `package-materialization/single-package.t`. Package-specific indexing
  still has install/scope consumers; keep its coverage while those
  remain. Remove a source/test pair together or identify equivalent
  surviving coverage.
  A fixture merger must preserve independent cold metadata requests in
  `library-enabled_if-with-package.t` and cold package materialization in
  `single-package.t`; do not warm one entrypoint with the other.
- [ ] D13 after fixture behaviour stabilises: split the remaining DP
  cases by fixture, including moving the scoped JS compiler-selection
  case to `jsoo/`. Keep its workspace and mounted controls in standard
  `runtest`. Split RP into directory rules, warning policy and install
  parity without changing output. RP's middle cases run in `_build/host`
  only because an earlier case replaced the contexts.

#### Backend-fixture boundaries

Use explicit stable matrices, not blanket native/opaque parity. Keep
requester roles, cold/warm controls and backend-specific assertions.
No helper options or generic runner are planned.

- Retain the external-prefix/OCAMLPATH controls in `pkg-pform/installed.t`
  and `installed-missing-file.t`; managed providers do not cover that
  lookup. Directory pforms differ by backend (directory entries versus
  installed leaves); do not impose parity when sharing producer fixtures.
- Deferred: CLI companion PATH (`exec-dependency-binaries.t`,
  `bin-narrowing/dune-exec.t`) needs the environment/discovery contract.
  E2E explicit/autolock tests need a repository fixture selector: their
  Dune tarballs are already native; source(copy) would bypass solving
  and fetching. Remove neither E2E scenario.

### S20. Test coverage improvements (remaining items unclaimed)

Kept on purpose, do not remove:
- SP's `cat _source/...` checks: a patch applied through a
  hard-linked or symlinked copy could modify the origin, so they can fail.
- DP's `(alias runtest) (enabled_if %{bin-available:..})` rule:
  its `enabled_if` is evaluated at rule generation and is the trigger the
  case is about.

Tests first, in their own commits; a test that exposes a bug records the
current output and a plan item, it does not fix the bug.

### S21. Remaining follow-ups from the fifth simplification audit

The independent items are exhausted. The remaining items are coupled to
the named migration or decision below. Preserve behavior and unique
assertions, re-estimate against the current base, and validate each
logical change with `@check @fmt` plus focused tests.

#### Source: take with, or after, another item

- [ ] C24 with A9: `package_scopes ctx candidates` is rebuilt by every
  `(context, owner)` partition and every `package_db` call, and
  `owner_libs owner` once per project of a partition: about N*P router
  databases for N requesters. Make it one node per context. Decide
  first whether a mount partition passing `owner_libs None` to
  `Rocq_scope.make` is intended.
- [ ] C27, blocked by S10's query migration: `Expander.artifacts` and
  `Expander.depends` stay memo-wrapped while `Action_expander.expander`
  builds them lazily from a `Pkg` closure for `Pkg.resolve` and
  `all_filtered_depexts`; making them plain would build every dependency
  on resolve. The `expand_action` alias is already gone.
- Not worth a commit alone (only if a further caller appears): a
  `Path.t` form of `Pkg_rules.source_location` for the three
  hand-written matches in `merlin.ml`, `ml_sources.ml`, `lib_info.ml`;
  building workspace package variables in `package_layout.ml` with
  `Lock_dir.Pkg_info.variables` (S11 rewrites that function).

#### Tests: gated on expected output (S19)

- [ ] S23 output-changing ideas (P; Ali), each with its surviving
  assertion named by the S23 agents: `mounted-projects.t` cut the
  undeclared-provider and vendored diagnostics to their `Error:` line
  (-8) and drop the workspace control before the disabled-vendored check
  (-3); `bin-narrowing/multi-package.t` print the identical native/opaque
  `show_denied` output once (-20); `bin-narrowing/transitive-deps.t`
  compare opaque against native (-5); `package-deps-run-binary.t`
  `sort -u` the four identical "from provider" lines per cell (-8..-12).

- [ ] V7 (-11) `pkg/portable-lockdirs/
  portable-lockdirs-dedup-manifest-errors.t`: the unconditional
  `mkpkg foo` case used to test error deduplication; it now succeeds
  and asserts what `workspace-deps/solver-workspace-dep.t` and
  `pin-stanza/cycle.t` assert. Keep the macos-only case (the only
  solver-level platform-conditional workspace dependency).

## Supporting audit notes (not an independent work queue)

Task sections and pending decisions above are authoritative. These notes
retain investigation leads and traps; older line numbers and size estimates
are not current priorities or permission to change behaviour. Re-read the
current source/test pair before acting; do not reopen closed experiments
without new evidence.

### A6. Discovery versus opaque build permission in `Artifacts`

`visible_packages` and `package_deps` distinguish discovering native
providers from permission to build opaque providers. Do not fold them just
because some call sites combine them; the accepted library policy does not
change binary lookup. `visible_packages = All` does not authorise building
every opaque package: explicit deps plus compiler packages supply permission.

During S7/S10, also check `Expander.add_package_deps`: it evaluates a
package-list builder and drops collected facts. Establish whether its
dependencies need recording before changing this path.

### A9. Inventory for S7/S11: library resolution in `scope.ml`

Use this as S7/S11's structural inventory, not a line-count target.
Check the current implementation as each consumer moves:
- three stacked databases: workspace and mounted public libraries, then
  `opaque_libs`, then `installed_libs` with a `mounted_libs` parent;
- `owner_db` builds a database per owner, with and without the owner
  itself (`include_owner`), plus a `by_package` scope table;
- an opaque library is resolved by building each candidate package
  (`opaque_libs` evaluates the package's dependency action) while
  libraries are being resolved;
Owner-specific opaque eligibility and provider dependency scopes remain
necessary: scanning an opaque package builds it. Do not extend the native
public-library rule into unrestricted opaque forcing.
Keep `Findlib.lookup_and_load.resolve_package`: package-specific DBs
may have an explicit owner with `lock_dir_active = false`;
`pkg/package-library-closure.t` fails if that owner is discarded.

### A10. `Lib` closure checks for several providers of one name

`lib.ml` adds a `package_scopes` table on every database and a
`src_dir` comparison in `find`. Two mounts can no longer carry the same
public name (each mount is masked to its own package), but opaque
packages still can: installed findlib names are arbitrary
(`pkg/package-library-closure.t` has a workspace and an opaque provider
of one name). So the checks stay; no test exercises the ppx key's
`opaque_packages`. The by-name closure check was dropped (Ali,
2026-11-26): only a hand-made lock with two packages installing one
findlib name could reach it.

### A11. Audit new partial functions (planning agent; both parts read)

Every added site was read; the reachable ones are fixed with tests.
Unsure, to read: `Build_system.directory_target_contents_opt` raises a
code error when its directory is not under a directory target (callers
in `pkg_rules.ml`, `source_tree.ml`); `Partitions.find_loaded_project`
expects a context path and does not fall back to the root project for
`_build/_private` or `_build/_fetch`. Check callers when moving them.
- Check the `(pkg enabled)` / opam-context boundary when moving calls to
  `Lock_dir.lock_dir_active`; do not make invalid-context errors eager.

### T. Tests (read by an agent; V = checked by the planning agent)

Paths are under `test/blackbox-tests/test-cases` unless they start with
`test/`. Line numbers are the agent's and unchecked unless marked V.

Behaviour changes hidden in updated expectations. Decide before cleanup:
- T1 (V). Lock-dir binaries are no longer found without a package scope.
  (`dune exec mybin` is exempt and prints `from foo` as on main, see
  the binary lookup decision.)
  `pkg/bin-narrowing/lockdir-deps.t`: the rules that ran `mybin` from a
  locked package are now disabled and the expectations assert the
  outputs do not exist. Fixtures in `describe/describe_location.t`,
  `pkg/installed-binary.t` and `pkg/bin-runtime-deps.t` were edited (lang
  version raised, a scoped package added) so their old output still
  holds. Decided: see "Binary lookup" under "Decisions in force".
  Consolidate the overlapping unscoped cases in
  `pkg/bin-narrowing/lockdir-deps.t` with this decision, not as an
  independent cleanup task.

Implementation details kept as their case's subject: cookie checks of
what a lookup builds, `_build/_fetch` in `unused-locked-packages.t` and
`mounted-local-backing.t`, the host-context `.ppx` counts in
`mounted-rule-parity.t`.

Redundant or misplaced:
- T16. Beyond the overlap candidates of the second test audit: "no
  context named default" is asserted three times and negative binary
  lookup four times.
- T18. `pkg/relocatable-compiler-sandbox.t:161-179` copies vendored flags
  into the test.

### Second test audit (overlap, sense, coverage)

Four read-only reviewers plus the planning agent; nothing was run. V =
checked by the planning agent against the files. Abbreviations under
`pkg/`: DP `mounted-dune-package.t`, RP `mounted-rule-parity.t`, PR
`mounted-projects.t`, SP `mounted-source-preparation.t`, PM
`mounted-promotion.t`, OS `mounted-opam-sources.t`, SL
`mounted-source-lookups.t`, MF `mounted-menhir-fallback.t`, CC
`mounted-compile-commands.t`.

Need Ali (expectations that pin an undecided or surprising behaviour):
- T21 (V). In `pkg/bin-narrowing/ambient-path-fallback.t`'s warm case,
  `%{bin:mybin}` prints "from system" but `(system mybin)` prints
  "from lockdir" after an explicit dependency builds the provider.
  `resolve-program.t` extends T1 to unscoped `(ocamllex)`/`(menhir)`.
- T24. Keep DP's scoped JS compiler-selection checks and
  `jsoo/compiler-identity.t`'s per-directory `(env (binaries))` checks.
  Preserve compiler selection, separate archives, shapes and shared
  runtimes, and host-tool selection when reworking D13 or
  `jsoo_rules.ml`.

Vacuous assertions (delete or replace):
- T25. "No writeback" checked in the `(source (copy ..))` origin, where a
  promotion would never land: PM 52, 63, 130, 158, 213; RP 311-312,
  562-563; DP 376, 400, 413. Use an empty `dune promotion list` and one
  `test ! -e .pkg`.
- T26. Tripwires that are never demanded: DP 266/291/311/329/336
  (`src/unrelated`), DP 569-572, PR 66 `(libraries missing)`, SP 205, OS
  `unused`; also RP 46, 401, 439, 512, 556, 595. The remaining
  `_install` checks are on packages whose recipe works; keep them.
- T27. Claims with no observation: RP 125 (edit never observed);
  PR 360-378; OS 51-72.

Overlap candidates (V unless noted), subject to S19's kept controls
and source/test-pair checks. Between the new files:
- native build, recipe not run: DP 1-35; drop RP 34-46, OS 51-72, DP
  193-201 tail, DP 263-267.
- substitute stays opaque: SP 123-138; drop SP 106-121, OS 1-49's use.
- non-default context: PM 197-214; drop RP 115-130.
- no writeback: PM; drop RP 296-315, 558-563.
- several packages from one snapshot: PR 1-45; OS 1-49 and RP 48-87 only
  reuse it. `@pkg-install` META: DP 37-44 or PR 104-112.
- type error in a mount: SL 84-104; drop RP 412-421.
- broken generated stanza: DP 159-170 and SP 294-310 (both T8).
Possible overlap with tests on main; retain workspace parity controls
when they establish the mounted policy rather than merely repeat output:
- RP 317-401 = `vendor/warnings.t`, `vendor/alerts.t`,
  `private-modules/private-module-excluded-by-modules-field.t`.
- RP 209-244 = `stanzas/copy_files/test6.t`; RP 441-460 =
  `dune-project-meta/duplicate-dependencies.t`; RP 467-512 =
  `install/install-glob/install-glob-relative.t` (reviewer).
- DP 339-414 = `opam-directory/generate-opam-file.t`,
  `doc-dependencies.t`; DP 104 = `project-root-env/`.
- PM 178-195 = `promote/promotion-list.t`; OS 74-107 ~
  `pkg/dep-section-variables.t`.
- DP 771-913 ~ `pkg/package-library-closure.t`,
  `package-materialization/`.

### Deferred reductions (not independent tasks)

Correctness gates and the S7/S10/S11 migration take precedence over these
old line-saving proposals. Re-estimate only after choosing the submission
seam and moving consumers; do not restore text solely to match main.

- B1 (A9/F8): keep the package-specific opaque/provider scopes required by
  the accepted visibility policy. Several-provider checks (A10) and jsoo
  compiler identity (T24) remain required.
- B2: digest-era restoration proposals are superseded by S10. Inventory
  remaining `Pkg_rules` adapters before removal; preserve workspace
  dependencies and the agreed dev-tool compiler sharing/isolation.

## Possible future optimisations (null build, measured 2026-11-26)

Not planned work; Ali picks. Benchmark: null `dune build` of dune's own
repository with a 122-package lock dir, fully built. Stack tip 14.4s;
with the `Env_path.cons_multi` fix (the stack's base) 4.4-5.0s; with B, G, E and
F below as well 3.6s. Without package management about 1.5s (Ali's
figure). The machine was noisy: each gain was measured against a
baseline run next to it, so compare within a row. None of these has
been through the test suite. Patches, profiles and the locked
benchmark scripts are in the session scratchpad under `perf/`
(`BRIEF.md` describes the protocol); the combination B+E+F+G is the
experiment change `ymmnmxzt`.

Trap for anyone profiling: do not start dune under `perf record --
dune ..`. perf changes the environment, one package rebuilds and
30,000 files are hashed, which looks like a hashing problem. Attach
to the running process (`perf/prof.sh`) and check the trace shows 0
actions and 0 file digests.

Worth taking, small and idiomatic (something computed per rule that
depends only on the directory, context or package):
- B (+7/-2, `ocaml_runtime_env.ml`; also main): `toolchain` and
  `melange` read `Env_node.base_env`, not
  `Super_context.context_env_by_dir`. They only look at `OCAMLRUNPARAM`
  and the two envs differ in PATH only. `context_env_by_dir` ran 12,419
  times for 108 distinct results. About 0.4s. Patch
  `p1-B-runtime-env-base.patch`.
- G (+27/-20, `partitions.ml`; prototype only): memoise
  `Partitions.package_closure_for_context` per (context, package).
  About 0.27s. Patch `p2-C1-memo-package-closure.patch`.
- F (+39/-10, `pkg_rules.ml`; partly main): load each recipe-built
  package's install cookie once, not once per consumer (442 loads of
  30 cookies, 173,433 paths turned into dependency sets; 445 loads
  become 62). About 0.2s. `pkg/install-cookie-load-dedup.t` already
  states the intent. Patch `p3-load-cookie-once.patch`.

Measured, little or no gain:
- C (+24/-14; also main): `context_env_by_dir` as a lazy field of
  `Env_node`. Same 0.4s as B and does not add to it; only `bin/`
  callers remain after B. A tidier home, not a speedup.
- D (+23/-13, `src/install/roots.ml`; also main):
  `extend_env_concat_path_vars` made linear like `cons_multi`. Within
  noise; matters only when an action env carries a long PATH.
- H (+51/-51, `artifacts_db.ml`; also main):
  `get_installed_binaries` creates an expander only for stanzas that
  install into `bin`. About 0.1s, near noise.
- GC `space_overhead` 500 to 1000 in `bin/main.ml`: 0.1-0.25s. Tuning,
  not a code fix; memory on real builds and in watch mode not
  measured. A larger minor heap gave nothing.

Not measured:
- `Dep.Facts.digest` hashes each file fact's path with a nested
  `Digest.string` inside a `Digest.Manual` (342,697 per build); feeding
  the path directly would save at most 1.7% but changes every rule
  digest. Main's code.
- 3,112 failing `opendir` calls per build under `_build/.actions`
  (`remove_old_sub_dirs_in_anonymous_actions_dir`), 0.3%. Main's code.
- `Install_layout.deps` rebuilds a dependency set from a file list per
  layout (0.6%); could be memoised beside `entries`.
- `Pkg_rules.listing` builds a full recursive file list per package
  only to test whether it is empty.

Ruled out, with evidence:
- The compiler dependency: removing it changes the null build by at
  most 0.05s. A directory target is read once per build, not once per
  dependent rule.
- Changing the binary-lookup rule for unowned directories: about 80 of
  310 native lookups per build come from them (`gcc` 32, `shellcheck`
  15, ..), and with all of them disabled 93 of 116 mounts still load
  through library resolution and install entries.
- Mount loading as a whole: about 0.55s, nothing read twice. The ten
  OCaml-syntax dune file processes cost CPU on other cores, no wall
  time.
- `Env.update`/`Env.add`: a plain map, no cached array.

What remains after these is engine work per rule (66,995 rule digests
and lookups, 496,457 file-dependency memo calls) and diffuse
allocation (630M minor words: fibers 27%, rules 24%, engine 20%). Main
does the same per rule; the graph is simply larger.

## Validation and submission

- [ ] Record the exact final code revision and main base, commands run,
  focused/full checks, exclusions and platform gaps. Historical passes
  (including the recorded post-rebase run) do not certify a later tip;
  recover their exact revisions from history rather than carrying forward
  a date-only green-suite claim.
- [ ] With Ali's approval, serialize one final
  `./dune.exe build @check @fmt @runtest` on that code revision.
  Documentation-only plan edits need `@check @fmt`, not the full suite.
- [ ] Confirm S7's behavioral gates, S10's dev-tool isolation/compiler
  sharing and host/target regressions. Retain the upstream #17089
  host-context export checks when adapting `pkg/exported-env.t`.
- [ ] Validate local package backings on macOS/FSEvents and Windows:
  relative workspace URLs, absolute external sources, configured build
  directories and file-link vs directory-link behavior.
- [ ] Re-lock and build fresh clones with the final binary (S14). Record
  source and lock revisions, any overlays, edits or fixups, and cache use.
  Report the exact target and whether benchmarks were built or run.

`bin-pform/windows-install-names.t` and `wasmoo/build-info.t` are outside
the default alias here; earlier checks found the same failures on main.
Reconfirm and report exclusions for the submitted revision, not as passing
coverage.

Report source size against main separately from tests and this plan:
it is the stack's measure (Direction), after the gates above pass.
Keep check logs and historical metrics in commit messages. Follow the
PR template; the submission should not contain `PLAN.md`.

Note in the PR: empty-provider PPX driver hashes change (one key
encoding).
