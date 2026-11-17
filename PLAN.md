# Coordination

## Direction and order

Keep the mounting model; complete demand-driven loading and prove isolation
before pursuing estimated line savings. Pending semantics remain Ali's
decisions, not cleanup permissions. The measure of the stack is the
absolute size of `jj diff --from main --stat` (Ali, 2026-11-17): deliver
the decided functionality in the fewest changed lines, simplicity next.
S17 records the older decision-gated reductions. S18 lists the current
abstraction-audit follow-ups, including local work and its prerequisites.
S21 lists the fifth audit's local simplifications of code and tests,
most of them claimable without a decision.

1. Settle the remaining environment and load-error contracts before
   changing their expectations.
2. Make S7 the principal completion gate, using S11's individually demanded
   package facts. Remove stored errors only after the eager consumers move.
3. Move ordinary consumers through S10, retaining necessary dev-tool
   adapters. Then converge traversal and test structure on the stable model.
4. Deliver the S14 Menhir overlay and revision-specific submission evidence.

S13's layout cosmetics, B2's legacy-text restoration and per-file patch stage 3
are not independent work streams on this path. S5/S6 record constraints;
do not repeat their closed removal experiments without new evidence.

## The stack

`snqzlotl` "prototype: flatten package builds into a single build graph"
sits on Ali's digest removal, bookmark `remove-pkg-digest` (seven
commits on main `51d6214a`, #17079). When Ali rewrites that branch, move
the stack onto its new tip (`jj rebase -s snqzlotl -d remove-pkg-digest`)
and abandon the old copies, or his commits show as divergent. Bookmark
`prototype/flatten-dune-v3` points at it and is pushed to `origin`
(move and push it again after squashing new work in). The 209 commits
squashed into it on 2026-11-17 are on `origin` only, at branch
`prototype/flatten-dune-v3-history` (`4098c409`, old base `b315a5a2`);
they share change ids with this stack, so do not fetch that branch into
this repo: it makes `snqzlotl` and `rlnpourw` divergent. Ali's
`spike/symmetric-library-discovery` (`d6dc0f3f`, inside that history)
moved to `origin` the same way. Workspaces parked on pre-squash commits
(`dune-doc-new`, `dune-r2-backing`) must be rebased by their owners. This
plan is the one commit above it (`rlnpourw`); plan edits are squashed
into that commit (`jj squash --into rlnpourw PLAN.md`), not added as
new commits. New work is a linear run of commits above the plan, ending
at `dune3@`; it is squashed into the single commit when it is settled.
The old 461-commit history, on the old base, is kept at bookmark
`stack-history` (`jj log -r '::stack-history'`). Workspaces still
parked on that history (`dune-nosp`, `dune-persource`,
`dune-ptd`, ..) must be rebased by their owners before integrating.
Facts from the move to the single commit, to keep in mind:
- Fetch targets are flat: `_build/_fetch/url-<digest>-dir` and
  `checksum-<sum>-dir` (#16945). `Fetch_rules.target` is gone; use
  main's `Fetch_rules.make_target ~kind`. The kind is `Kind.t`
  (`File | Dir`), not a polymorphic variant.
- `Lock_dir.source_kind` returns a result. A local source's kind is
  asked inside the fetch action, so a missing local path only fails what
  needs it.
- The `.pkg` root under `_private` generates rules for every digest
  except the project's (told from `DB.Pkg_table.project_entries`, no
  resolution): project packages are mounted.
- Rule plugins (#16760) are keyed by the project's `Path.Local.t` root.
  A plugin of a mounted package runs in the package's prepared source.
- From dune 3.26 `(plugin ..)` is a rule plugin and the site stanza is
  `dune_site.plugin`.
- Accepted (Ali): for a mounted package a missing source is reported
  before its dependencies are built (`pkg/build-single-package.t`).

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
  Before committing: `./dune.exe build @check @fmt`; code/test changes
  also need affected regressions/focused tests. Docs need no tests.
  No full suite or expectation promotion without explicit approval.
- Tests precede fixes in separate logical commits.
- Integrate finished commits into the shared stack, preserving others'
  work: e.g. `jj rebase -r '<your-revset>' --insert-before 'dune3@'`.
  Verify with `jj log`/`jj status`, resolve conflicts and validate integration.
- After owned commands stop, `jj workspace forget <name>` from another
  workspace and remove only your temporary directory. Never delete others'
  workspaces/shared data or lock files to bypass contention.
- Report integrated hashes, checks and gaps; update this shared plan.
  Squash plan-only changes into `rlnpourw` as soon as they are made,
  explicitly selecting `PLAN.md`; do not add plan commits or mix them
  into the code commit. Agents that cannot be messaged only see the
  plan commit.
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

## Active: S7 lazy package loading (dune3 agent)

Ali (2026-11-13): resume. S7 under "Open work" is the task list and
completion gate. Workspace `dune-s7-rule2`, `/tmp/dune-s7-rule2.MOvh/ws`.

## Active: S10 consumer migration (Claude)

Workspace `dune-s10`, `/tmp/dune-s10`. Base question still with Ali; until
then, on the current stack: `find_package`, `resolve_installed_file`, env,
toolchain and binaries. Locked dependency edges and names come from
`Pkg_sources.dependencies` and the name-keyed `Pkg_sources.candidates`.

## Active: install cookie duplicate entries, fix for main (dune-cookie agent)

Workspace `dune-cookie`, `/tmp/dune-cookie.fu2g8G/ws`, based on an
earlier main (`fc3ada5e`), not on the stack. Bookmark `install-cookie-once` (two
commits: `wxlokpst` test, `rkwovvmw` fix) ports "fix(pkg): record an
installed file once in the install cookie" to main; `@check @fmt` and
`pkg/install-action.t`, `pkg/dep-section-variables.t` pass. On main the
duplicate only shows in the cookie (`install-action.t`); locked packages
do not reach the install-layout collision there, so the stack's
self-collision case was not ported.
- [ ] Ali: push the bookmark and open the PR; then add
  `doc/changes/fixed/<PR>.md` and remove this entry.
- When the stack is next rebased onto main after the PR merges, drop
  the stack's copy of the fix (`pkg_rules.ml` `declared` filter,
  `install-file-run` version 2).

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
source directory links are not traversed. Supporting directory links is
separate work requiring approval, not a local-backing compatibility fix.

The earlier workspace `dune-r2-backing`, `/tmp/dune-r2-backing.RTgE/ws`,
is preserved and paused. Do not integrate its scanner/readlink/symlink
changes wholesale; they were designed for the broader, deferred policy.

## Parked: no-narrowing spike, libraries (NOT for integration)

Workspace `dune-nosp`, `/tmp/dune-nosp.Mk7g/ws`, is on the old history.
Do not integrate it without its owner's rebase and a new review against the
accepted visibility policy below. Opaque eligibility, provider dependency
scopes and private-library boundaries must not be removed.

## Pending decisions (Ali)

In rough order of what they unblock:
- S7: `%{lib-available:x}` raising when `x`'s package fails to load.
  Confirm before replacing stored-failure expectations.
- S7 rule 2 consequence: a directory that belongs to no package sees no
  locked binary, native or opaque, as decided for binary lookup. So
  `dune fmt` from an unscoped root no longer finds an ocamlformat that
  is only a project dependency, and an undeclared `(run app)` of a
  native locked app fails
  (`pkg/ocamlformat/ocamlformat-avoid-taking-from-project-deps.t`,
  `pkg/sites-plugin.t`). Keep, or give `dune fmt` and unscoped
  directories the project's packages as their declared set.
- Shell lookup (T21): keep or remove the dependence of `(system x)`
  on an undeclared provider having been built earlier. Decide separately
  from `%{bin:x}` and `(run x)` lookup.
- Exported environment (T2): append ambient install-path variables, or
  replace non-PATH variables as on main. Decide unset versus set-but-empty
  components explicitly; do not let executor helper reuse choose policy.
- P5: whether `%{make}` in an exported-env value uses dune's ambient PATH
  or the host context's PATH. This is separate from T2 and T21.
- S16.F4: rewrite compiler `File "..."` lines for mounted sources, or
  leave the short path. No compiler-output rewrite before this decision.
- Marked "needs Ali" where they stand: T19 (pin cycle no longer reported
  when locking, with the solver/exclusive-dir changes), the S16 odoc
  `is_vendored` item, S17's A3 and `Package_mask`, S19's D11
  and the DP opam/odoc control.

## Decisions in force (Ali)

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
  `Pkg_sources.package_of_artifact_path` (build path) or
  `package_of_dir_in_context`. Path for a message:
  `Pkg_sources.source_location` in the rules,
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
- Keep compiler lookup as decided (Ali, 2026-11-14). For an opaque
  library: the package named like it, then the compiler's libraries,
  then the eligible opaque scan. Keep compiler priority for binaries.
  In a locked context the library parent uses only the compiler-reported
  standard-library directory, not a per-package findlib root or project
  OCAMLPATH imported through an absolute OCAMLFIND_CONF.
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
- Binary lookup (Ali, 2026-11-13): the regular lookup, as on main. Only
  its lock-directory step is narrowed: a lookup never builds an opaque
  package to see what it installs, so that step builds the opaque
  packages the asker declares (its package's dependencies, an action's
  `(deps (package ..))`) and the compiler's, nothing else. Native
  packages are found by declared name. The rule lives in
  `Pkg_rules.lock_dir_binaries`. Not optional: main's "build every
  package" is a dependency cycle once an opaque package depends on a
  native one.
  Exception (Ali): a program named on the command line (`dune exec
  prog`) has no declared list, so when the workspace and the native
  packages do not define it, every opaque package is built and searched
  before the ambient PATH (`Super_context.resolve_command_line_program`).
- A natively built locked package has entries under the workspace
  `_build/install/<ctx>` (Ali, 2026-11-13: accepted). Its `.install`
  file names them as any package's does; they are lazy and `dune
  install` only installs workspace packages. Opaque packages are not
  there; nothing needs the directory to be complete.
- Raise load errors on demand, not through catch/store machinery (S7).
- Every locked package with a source is mounted. Modified sources use
  one sandboxed rule outside the mount and per-file copies into it. A
  recipe runs in a sandbox copy of its mount, without the leading
  patches preparation already applied. A recipe that transforms its
  source further (a substitution, a later patch) keeps the package off
  dune rules until S7.
- Workspace/lockdir checks run at rule generation, not `Lock_dir.get`.
- Keep mount-required Shared_locks (private locks without a default
  context), jsoo Config/runtime keys, cinaps/mdx mounted listing,
  the workspace load in `Dune_load` below `Pkg_sources` (partitions in
  `Partitions`); do not re-audit.
  jsoo's digest-keyed global table is a later redesign candidate.
- `action_project_root` is a `Path.Local.t` (the engine appends it to the
  context root).
  Sandbox dependency linking is back to main; reproduce renewed
  directory-overlap failures before changing it.
- No `doc/` changes. The `dune-project` `depends` move is separate on
  main (`kvonvwnn`, not pushed); those files match main here.
- Keep compile-commands changes until #16977 merges and we rebase.

# Open work

Parked principle for the native-or-recipe decision (Ali, 2026-11-12, not
to be worked on yet): the build instructions say how the source is
prepared; after preparation, a `dune-project` declaring the package is
what decides between dune rules and the recipe. The vendored exception
is gone (S14); what still sends a package with a `dune-project` to its
recipe is a recipe that transforms the source (a substitution, a later
patch).

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

Keep the supplied `source_files` / `source_sub_dirs` inventory and
`directory_target_contents_opt`. Mounted files are ordinary rules but
still need source-versus-rule arbitration for fallback/promote rules and
directory targets. Folding source directories into build-only directories
would reject valid directory targets over mounted source directories.

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

### S7. Demand-driven loading (active; principal completion gate)

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
- A context load is the workspace; a mount is one
node holding its projects, dune files, scopes and public libraries
(`Dune_load.mount`, `Scope.DB`'s partitions, `Artifacts_db`'s
per-package binaries). A library is looked up in the workspace, then in
the locked package named like it (exact for dune packages; a
`deprecated_library_name` is owned by its old name's package), then in
the compiler's libraries, then by scanning eligible opaque packages
(`pkg/library-lookup-order.t`). Native public lookup is requester-independent;
opaque eligibility is the declared closure for a package-owned requester,
or unrestricted for an unscoped one. The compiler's copy wins over an opaque
package that installs a library of the same name without being named like it.
A miss can still build every eligible opaque package; `dune exec prog`
searches every locked package by name when the workspace does not define it.
Not fixed: a flat install section (`_build/install/<ctx>/bin`, `man`,
..) or `@default` still loads every candidate, since mounted entries
there cannot be found by directory; so a full build still classifies
every package each run.
Update the saved/stored-failure prose in `mounted-dune-package.t` and
`mounted-source-preparation.t` with this error-handling change, rather
than as a separate cleanup.

Build S11's metadata-only index before moving the remaining consumers;
source-dependent facts must be individually demanded. Index candidates by
package rather than repeatedly re-keying lists. Avoid duplicate loader/scope
tables, repeated closures, linear project lookup and memos over memos as
part of this migration, not as competing cleanup tasks.

Traps for consumers of the lazy database: a local library id carries
its local name, so look an id up by its directory's partition, never by
name. dune's own bootstrap rule reads mounted packages as installed
findlib packages through OCAMLPATH (`boot/dune`, nested dune), so the
generated dune-package/META of a mount must be complete.

Work in order, coordinated with S10/S11; `pkg/unused-locked-packages.t`
records the current behaviour and flips as the steps land:
- Left by rule 1: a native package's exported env (`setenv`) is now
  applied to the context env with paths under its unused prefix (was
  dropped).
- [ ] Install scheme (`Install_rules.scheme_per_ctx_memo`): approximate
  each locked package's rules by its package-named section directories
  (`lib/<p>`, `libexec/<p>`, `share/<p>`, `etc/<p>`, `doc/<p>`) plus the
  flat sections minus other packages' directories, so that loading
  `lib/<p>` loads only `p`. Test first: a workspace package with
  library-only install entries, `@install` with an unfetchable unused
  package present.
- [ ] Then remove `Memo.collect_user_errors`,
  `Error_handler.without_reporting`, `Dune_load.package_errors`,
  `Mounted.kind_exn` and stored `Error _` branches (after the
  `%{lib-available}` decision).

Observable exit criteria:
- Unrelated workspace targets do not contact unused HTTP/Git sources or
  fail on an unused broken package (`unused-locked-packages.t`,
  `git-source.t`, `source-with-directory-symlink.t`).
- A named native-library or compiler-library hit does not load unrelated
  mounts. Install-directory lookups do not enumerate every mount.
- Demanded failures through package deps, libraries and package pforms
  retain their file location and "required by" chain.
- Deprecated library names, declared binary visibility, host/target
  selection and dev-tool sharing/isolation still work.
- Only after those consumers are lazy may the stored-error machinery go.

Measured, not broken down:
- Null build of dune itself (Ali's trace, 2026-11-12): 4.1s, of which
  processes are about 1s; the other 3s are dune's own work and are not
  broken down by the trace (needs profiling). All 143 locked packages
  logged "Prepare package sources"; after S7 rule 2 only a flat install
  section or `@default` loads every candidate (not re-measured). Install
  cookies were loaded 520 times for 28 files before the toolchain was
  memoized; not re-measured.

## Rules

### S8. Follow-ups to source preparation

- [ ] Simplify early conditional-patch evaluation following `enabled_if`.
  Source preparation and recipes already share `Slang_expand.eval_blang`;
  investigate common variable resolution and dependency handling, not a
  second boolean engine. Preserve metadata-only conditions without
  building dependencies, false-condition skipping of missing patch files,
  preparation order and lazy loading. Conditionality alone is not a
  reason to select the opaque backend (B4). Concrete correctness work is
  S18.A1-A2; keep the detailed tasks there rather than duplicate them.
- Mounted substitution waits for S7; otherwise eager loading would make
  source preparation build dependencies on every cold build.
- OCaml-syntax dune files and mounted rule plugins still require a real
  prepared directory; preserve private writable execution and origin
  immutability. Per-file patch stage 3 remains deferred.
- Whole-package re-preparation after one changed file is an accepted cost;
  note it in the PR. No rebuild-granularity comparison test exists.
- Rewritten `pkg/mounted-source-preparation.t` was not run against the
  previous implementation.
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

This is the consumer-migration phase of S7/S11. Ordinary consumers still
use legacy `Pkg_rules.Package_universe/Resolve/Pkg.t` adapters; they are
not dead. Inventory their callers and choose the submission adapter seam
before B2 restoration work. Do not restore legacy text only to replace it.
Target: installation universes only for dev tools and unshared deps.

Seam (Ali, 2026-11-17): no legacy path and no package digests, neither
`.pkg/<digest>` paths nor the digest tables. Do not build on
`Pkg_digest`, `DB`, `Resolve`, `Pkg.t` or `Package_universe`.
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
- `dune pkg print-digest` is removed (Ali).
- The stack is based on Ali's digest removal (`remove-pkg-digest`): `DB`,
  `Resolve` and `Pkg.t` are keyed by name in isolated universes
  (`DB.get universe`), dev-tool packages live at
  `_private/default/.dev-tool/<tool>/<pkg>`, and nothing is shared
  between universes. A context's `Dependencies` universe is entirely
  mounted lock candidates, so `Resolve` takes the candidate's paths and
  the private `.pkg` directory has no rules. The base shares no compiler
  between tools; that rule is still to do.
- Order: (1) candidates belong to a package set (context or dev tool)
  that chooses the mount path; (2) the recipe rule takes a candidate and
  its dependency layout, not a `Pkg.t`; (3) dev-tool rules,
  `dev_tool_env` and `dev_tool_ocamlpath` use it, and the lock-time
  compiler rule is widened; (4) ordinary consumers below; (5) delete.
- [ ] Move mounted opaque recipe/dependency/cookie/installed-file queries
  to selected candidates and canonical mounted paths.
- [ ] Migrate ordinary toolchain/binary/env/OCAMLPATH/prefix/closure
  consumers to per-context package facts.
- [ ] Preserve dev-tool matching-digest sharing and differing-version
  isolation, with/without project locks and non-default contexts.
- [ ] Preserve depext reporting (`print-digest` is gone, see "Seam").
- [ ] Only then shrink/delete legacy DB/Resolve/Pkg/`.pkg/<digest>`
  execution machinery; report remaining callers first.

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
Index lock/workspace metadata without fetching or classifying sources.
Demand source-dependent provider, project and installed facts individually
per context/package; one provider query replaces repeated kind checks.

Preserve dependency semantics, not just similar-looking code:
`Dune_load` follows authored dependency names, while
`Package_layout.materialize_dependencies` resolves formulas.
Check solver environments, alternatives, optional/platform dependencies
and workspace cycles before sharing those calculations. Do not merge them
merely to reduce lines. Preserve dev-tool isolation and digest sharing.

Exit: constructing the index touches no package source, independent
consumers reuse the demanded facts, and neither closure nor version lookup
reintroduces a context-wide classification pass.

### S13. Build-directory layout (deferred unless needed by S10)

Mounts are at `_build/<ctx>/.pkg/<name>`; dev tools keep
`_build/_private/<ctx>/.pkg/<digest>`. Still open: the prepared-source
(`_private/<ctx>/.dune-source`) and legacy dev-tool layouts; `source`
and `extra_source` in `Paths.of_root` are unused for mounts.

### S14. Mounting model (decided) and the Menhir patch

Decided (Ali, 2026-11-13): one mount per locked package, masked to that
package. A source that vendors a project with its own package is
mounted like any other; nothing special is done for vendored packages,
so a vendored *public* library is masked out and not found
(`pkg/mounted-projects.t`). The native-or-recipe condition has no
vendored case. Vendored code without packages (private libraries, the
common case) loads natively.

Menhir is the known package with the masked shape, and it must be
patched for dune itself to build: 7 of the 145 packages in dune's lock
reach it (`js_of_ocaml`, `js_of_ocaml-compiler`, `lambda-term`, `logs`,
`mdx`, `melange`, `utop`; `logs` through a satisfied optional
dependency), so the default alias fails with `Library "vendored_pprint"
not found`.
- The patch makes Menhir's copies of `fix`, `bitsets` and `pprint`
  private libraries of Menhir's own project and drops the `vendored_`
  prefix and its alias modules; the directories stay `vendored_dirs`.
  In `~/repos/menhir` (local only, nothing pushed): bookmark
  `unvendor-master` on upstream master, `unvendor-20260209` on the
  release commit. Lock patch:
  `git diff --no-renames 20260209 unvendor-20260209`, applied as the
  first step of the `menhir` package's build action (only that one of
  the five Menhir packages needs it).
- Checked: with the patch in the lock, the full default `dune build` of
  dune passes with the stack's dune (clone of the fork's
  `package-test-dependencies` branch, dependency hash removed from
  `lock.dune`). Under plain opam with released dune 3.25.0 and the
  genuine `pprint` installed in the switch, the five patched packages
  install without conflict. Not checked: tests in the clone; `fix` and
  `bitsets` installed alongside; the `houblix` tests of Menhir.
- [ ] Submission gate: deliver a durable overlay for dune's own lock.
  A private checkout or relock fixup script is not completion. Plain
  `dune pkg lock` must retain the patch and a valid dependency hash.
  Both routes below have worked in local testing (full
  default build passes): (a) `fixup-lock.sh` in Ali's test checkout
  runs `dune pkg lock`, drops the dependency hash and adds the patch to
  the `menhir` lock file; lost on every relock without the script.
  (b) An overlay entry `packages/menhir/menhir.20260209/` (upstream's
  opam file plus `patches`/`extra-files` and `files/unvendor.patch`):
  plain `dune pkg lock` then yields the patched lock with its hash
  intact. Prepared and tested against a local clone of
  `ocaml-dune/opam-overlays` (temporary, `/tmp/opam-overlays.*`; not
  pushed). To do: open the PR on the overlay repository. Pinning
  `menhir` to the patched branch does not work: its `dune-project`
  declares no dependencies, so the solver drops the sibling packages.
- [ ] Upstream: Menhir's issue tracker
  (gitlab.inria.fr/fpottier/menhir), mailing list, or the maintainer.
  Releases are irregular (gaps of 6 to 14 months) and every published
  version keeps the old shape, so upstream alone does not unblock us.
- Note in the PR: a locked package whose source vendors a public
  library in its own package fails to build until it is patched.

Considered and not pursued: sharing vendored projects between the
mounts of one source (the source identity is free, `Candidate.
source_root` is the `_fetch` target, but it needs a representative, a
visibility exception and a rule for patched copies); one mount per
source (experiment `zsuxuymo`, workspace `dune-persource`: Menhir built
once natively, 5 `pkg/` tests failed, and a directory would no longer
have one owning package); vendored packages private to their mount
(experiment bookmark `exp-vendored-native`, workspace `dune-s18b`: net
zero lines, passes `pkg/` on a fixture, not reviewed).
In dune's lock only three sources are shared by several packages
(Menhir 5, js_of_ocaml 2, odoc 2).
Upstream context: #4070 (why Menhir renamed its vendored copies), #7058
(package subsetting for vendored directories, unimplemented), #3911,
#6830.

## Diff hygiene

### S15. Remove unrelated changes from the diff

Drop compile-commands changes after #16977 merges/rebase. Other listed
items are resolved; do not repeat audits. For new suspects, inspect
`jj diff --from main --stat`: state a mount-related need or revert.
For main, separately: `Lib.DB.with_parent` keeps `instantiate` closed
over the parentless database, so libraries it instantiates resolve
deps without the parent scope (the stack restored main's text in
`sovrlpqv`; only the utop dev-tool lock dir reaches it, untested).

### S16. Generalise workspace source paths in the rules (in progress)

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
   `Pkg_sources.source_location dir`, and its loaded directory
   `Dune_load.source_tree_dir ~dir`.
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
  remains the pending S16.F4 decision; do not add rewriting yet.
- Package version in build-info and `%{version:..}`, VCS: parked; leave
  as today.
- odoc: a mounted package's docs build through its mount, minimal
  support only; no further odoc features or tests for mounts.

Remaining work, one feature per commit, test first:
- S16.F4 (deferred pending Ali): whether compile errors show the full
  build path. Not started: the compiler runs in `_build/<ctx>` and prints the
  relative path it was given; dune has no layer rewriting compiler
  output. Options put to Ali: rewrite the `File "..."` lines of failing
  commands whose directory is in a mount, or leave it. Test:
  `pkg/mounted-source-lookups.t`, last block (and the menhir error
  location in the same style).
- [ ] Remaining source lookup: `format_rules.gen_format_alias` still
  asks the workspace tree; fine while mounts get no formatting rules.
  `warning_emit`'s `Source_dir_or_enable of Path.Source.t option` is
  workspace-only by construction since mounted escapes are errors.
- [ ] Needs Ali: `odoc/odoc_new.ml` asks the workspace tree
  `is_vendored` for every project, so a mounted project answers "not
  vendored". Left out under the minimal odoc scope unless wanted.

Leave alone:
- `Path.Source.t` used as a context-relative name: `top_module`,
  `bootstrap_info`, `melange.ml:57`, `super_context:23`, `scope:775`,
  `module.ml:34`, `install_layout:66`, `install_rules:1184`,
  `context.ml`, `pkg_rules`, and `extract_build_context` uses that only
  read the context name.
- Workspace-only by nature: `cram/`, `lock_rules`, `lock_dir`,
  `dune_load`, `loaded_project` identity, `pkg_sources`,
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

Not testable yet: mdx, cinaps and melange in a mount (a mount only sees
libraries from the lock dir, so their runtime libraries are "not found"
first); utop has no mounted entry point from the CLI; no rocq installed.
Foreign `include_dirs` already works.

Done when: `grep -rnE 'Source_tree\.(find_dir|files_of|nearest_dir|is_vendored)|drop_build_context' src/dune_rules`
lists only the "leave alone" files; every generalised feature has a
mounted test. State on 2026-11-16: the grep lists the leave-alone files,
`format_rules` and `warning_emit` (both above), `source_deps` and
`melange_rules` (mount-aware), and `odoc_new` (needs Ali).

#### Path model follow-ups (unclaimed unless marked; ask the dune3 agent)

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
with the tests named; no full suite. Nothing unblocked is left: the
listed items (A3, A11, D3, about 65 lines) need Ali, and the "blocked"
lists need their named decision or a fresh audit against the accepted
contracts; old estimates are not permission to remove required plumbing.
Labels are local to S17: cite them as `S17.A3`. A bare `A6`, `A9`,
`A10`, `A11`, `B1`, `B2`, `B4`, `C2` or `D11`-`D13` anywhere else in
this file is the older audit note, deferred reduction or test item.

Packages (`pkg_rules.ml`, `package_layout.ml`, `pkg_sources.ml`):
- [ ] A3 (-12, needs Ali) `Action_expander.for_recipe` re-adds the seven
  variables of `Pkg.base_env`. Not equivalent (dune-min-a3, 2026-11-17):
  for a toolchain compiler without the relocatable marker, `paths.prefix`
  is the toolchain directory while `base_env` uses `target_dir`, so the
  stack's mounted build sets `OPAM_SWITCH_PREFIX` to the toolchain dir;
  main (and the stack's non-mounted expander) use `target_dir`. No test
  shows it. Either restore main's value (superpose `Pkg.base_env pkg`,
  -12, a fix with a test printing the variable) or keep it (superpose
  plus one `Env.add`, -8).
Blocked: `Expander.path` field and substitute version 5 (~22) on P5;
`Mounted.loaded`, `kind_exn`,
`collect_user_errors` in `pkg_sources` (~25) on the S7 stored-error
step; `Pkg.deps` as `dependencies >>| ignore` (~13) changes opaque deps
from the target directory to cookie+files, owner's call; the
`install-file-run` v2 filter goes at the rebase (dune-cookie).

Library and binary resolution (`scope.ml`, `lib.ml`, `artifacts*.ml`):
Re-audit any proposed removal of `Scope.DB.for_package`,
`Expander.for_package`, the partition's exclusive-package branch,
`by_package`, `create_db_from_stanzas`, `Found_or_redirect.owner`,
`Scope.public_libs`, `scope_host` and the ppx `opaque_packages` map
against the accepted visibility policy. Native-public symmetry does not
remove their opaque/provider/private-scope duties; old line estimates are
not removal permissions. Blocked on the `%{lib-available}`
decision: `errors`/`failed` in `owner_db`, the `Error` arms, `Resolve_
result.invalid` (~32). Not recommended: undoing the `analyze_binary`
helper extraction in `artifacts.ml` (duplicates 14 lines or changes
local-before-native precedence).

Loading, project model, source tree:
Blocked: the stored-error machinery in this area
(`Memo.collect_user_errors`, `Error_handler.without_reporting`,
`Dune_load.package_errors`, `main.ml` opaque check, ~50) on the S7
step; `Package_mask` by-id precompute (~20) conflicts with the
build-path message decision, needs Ali.

Rules and engine:
Blocked: the install scheme `package_errors` approximation (~15) on S7;
`compile_commands.ml` (~241) goes at the #16977 rebase (D12.a).

Read and found necessary, do not re-audit: `Lib.L.scope_dir`'s
`Code_error.raise` (not main's `assert`, AGENTS.md); `build_env_of_deps` is
main's text (A9 refuted: sharing it is +11); jsoo `Config.compiler`'s
"different backend" error (D8 refuted: a dynamic dep on the shared
runtime path reaches it); `lib_entries_of_package`'s empty-set match
(B12: removing it re-indents 28 lines); the two prepared-dir
computations (C4 refuted: a shared helper is +6, `eval_one` still
converts paths); `Lib.is_local` via
`Obj_dir.to_local` (B5 refuted: main's text calls opaque installs under
`_build/default/.pkg` local, 17 `pkg/` tests fail);
`owner_db ~include_owner:false` (B3 refuted 2026-11-17: with the
package included, a hidden library
lookup re-enters its own database forever, `pkg/findlib-meta-optional-
directory.t` hangs); the S5 engine inventory and
build-only root; the action-marker promotion property through
`print_diff`'s option; `warning_emit`'s context bag (net -32); the
install-entry `~source_tree_root` (mount-confined globs); `merlin`/
`ocaml_index` `~owner` (F8); `Backing` hash/equal, `File.parent_exn`/
`relative`, `Include_stanza` mount-root checks; `Dune_load`'s
reserved `.pkg` check; `Loaded_project.visible_packages`; `Dune_project`
`display_root`/`name_hum`; `Source_file_dir.rules`; `Context_libs` over
`default_ocamlpath`; `lib.ml`'s by-name closure check and `src_dir`
comparison (A10); `Findlib.create_for_package`; `resolve_command_line_
program`; `Install.Entry.Expanded.make`; `Paths.of_root`, `Layout.
materialize`, `layout_env`, `workspace_dependencies`, `compiler_packages`,
`DB.Pkg_table.project_entries`; `Copy_source`; the recipe executor seam
(S10); the `dune_pkg` bundle and the `Path.Local` type change (parked).

### S18. Abstraction audit follow-ups (Astra; unclaimed)

Static audit of the pre-library-discovery-integration source tree against
the actual stack base, not the moving `main` bookmark. Exact base/tip and
audit provenance are in the plan commit message. Source anchors below are
from that snapshot: re-read the current implementation and all callers
before claiming an item, especially after the library integration.
Three independent reviewers and a consolidator read the code; neither
suspected bug has an executed reproduction. These are tasks to validate,
not a verified deletion budget or permission to change semantics.
Feature-rule adapters received less exhaustive scrutiny than the central
loading and package interfaces.

Keep one bounded change per task/workspace. Bug regressions must precede
their fixes in separate commits. Preserve all existing observations;
focused validation is proposed below, not recorded as already passing.

#### Correctness first

- [ ] S18.A1: preserve environment scopes in residual recipes.
  Classification: suspected bug; high static confidence; bounded scope.
  Anchors: `src/dune_rules/pkg_sources.ml:284-288` (`flatten_progn`) and
  `src/dune_rules/pkg_rules.ml:3012-3014` (prepared recipe execution).
  Flattening removes `Withenv` wrappers, but the resulting residual AST
  is executed for opaque prepared sources. An overlay alone can select
  this path, so adding a lock-directory file or extra source can silently
  remove a recipe's environment. OPAM `build-env` uses this wrapper too.
  Preserve scopes while separating leading preparation actions from the
  residual program; classification must not discard executable semantics.
  Do not fix this by abandoning native conditional patches, replaying
  already-applied patches, or changing source/overlay ordering.
  First extend `pkg/withenv.t` with an otherwise identical opaque recipe
  plus an overlay and assert its environment value. Cover lock files and
  extra sources, and retain origin immutability. Then check the OPAM
  build-env cases and `pkg/mounted-source-preparation.t`.

- [ ] S18.A2: propagate unresolved condition inputs before Boolean coercion.
  Classification: suspected bug/new S8 evidence; high static confidence;
  medium scope because dependency staging must be designed.
  Anchors: `src/dune_rules/pkg_sources.ml:292-340` and
  `src/dune_rules/slang_expand.ml:81-96`.
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
  changed-line saving. Anchors: `src/dune_rules/dune_file.ml:538-584`
  and its caller at `src/dune_rules/dune_load.ml:43-54`.
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
  Anchors: `src/dune_rules/dune_load.ml:67-139` and
  `src/dune_rules/scope.ml:837-846`.
  Opaque packages receive empty parsed partitions, requiring empty-map
  handling in loading and scope lookup. Consider `None` for no parsed
  Dune partition instead of constructing empty facts.
  Do not remove the physical mount or its ordinary source-copy rules.
  S7 must first establish equivalent error propagation and safe workspace
  fallback under reserved `.pkg`; re-check the scope side after the
  accepted library-discovery integration.
  Check `pkg/mounted-eligibility.t`, `pkg/mounted-opam-sources.t`,
  `pkg/opaque-recipe-source.t` and reserved-`.pkg` controls. Treat this as
  an investigation, not an approved removal.

#### Boundaries that the simplifications must retain

- `Source_tree_file.Backing/File/Dir`: logical identity, physical reads and
  confinement are distinct concerns. Narrow construction, not the model.
- `Source_tree` versus `Source_file_dir`: authored-tree interpretation and
  source-copy rules/engine arbitration have different jobs.
- `Dune_load` (workspace load, main's module) below `Pkg_sources`, and
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
- Stored-error removal still waits for S7's consumers and the remaining
  `lib-available` decision; compile-commands removal still waits for
  #16977/rebase. Environment/PATH, mask and documentation policy choices
  are not mechanical simplifications.

Recommended bounded order: S18.A1 reproduction/fix; S18.A2 through S8.
Take A7-A9 only with their named prerequisites.
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
- S7 eagerness: lost "required by" lines in `unused-locked-packages.t`,
  `unavailable-package-source.t` and `source-with-directory-symlink.t`
  (a demanded source or load error names no requester).
- Narrowing (A9, Ali): `sites-plugin.t`, a natively loaded locked app
  cannot see `dune-site`, which is not in the lock directory. Fixture
  changed to a scoped package in `package-deps-run-binary.t`.
- S14 (Ali, accepted): `mounted-projects.t`, a source vendoring a
  public library in its own package fails natively with "Library .. not
  found". Fixed in the package (Menhir patch), not in dune.
- T19 (Ali): `pin-stanza/cycle.t`, a package cycle through pins is no
  longer reported when locking.
- Env (T2, Ali): `opam-package-with-setenv.t`, exported env updates now
  keep the initial environment.

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
  - PM "ordinary workspace promotion remains available": guards against
    the mount's no-promotion rule leaking to workspace rules.
  - RP relative globs: the second test audit's mapping to
    `install-glob-relative.t` is wrong (that test is about the `..`
    destination error).
  - DP's opam/odoc case compares against the workspace output with
    `cmp`; dropping the control needs new expected output (ask Ali).
- D11 (needs Ali): the remaining host/target cases each cover a
  different thing and are kept for now: RP's PPX driver in the host
  partition (now confined to its case), PM's promotion in non-default
  contexts, DP's jsoo host compiler (T24) and DP's package layout
  materialised in the host context. "Single user context" was read by
  the audit as "no host/target", but cross-compilation exists on main
  (`bin-narrowing/cross-compilation.t`). Delete only on Ali's word.
- [ ] D12.a after #16977 merges/rebase: remove the superseded
  compile-command source/test changes together (`lifecycle.t`,
  `namespace.t`, `copy-files.t` and CC's extra cases). Do not drop
  mount-specific coverage that still protects a retained source change.
  `copy-files.t` no longer tests that `dune build compile_commands.json`
  is (not) a target.
- [ ] D12.b audit source/test pairs before removing unrelated changes:
  `gen_rules_combine_tests.ml`, `library-enabled_if-with-package.t`,
  `package-materialization/single-package.t` and `expect-tests/scope/`.
  Package-specific indexing still has install/scope consumers; keep its
  coverage while those remain. The scope expect test also checks obsolete
  result retention, which blackbox metadata-output tests do not prove.
  Remove a source/test pair together or identify equivalent surviving
  coverage. Solver/exclusive-dir changes still need Ali's T19 decision.
  A fixture merger must preserve independent cold metadata requests in
  `library-enabled_if-with-package.t` and cold package materialization in
  `single-package.t`; do not warm one entrypoint with the other.
- [ ] D13 after fixture behaviour stabilises: split DP by fixture, RP into
  directory rules, warning policy and install parity, and move the
  standalone jsoo cases into `jsoo/` (keep their two output checks).
  Preserve controls and output. DP is eight independent fixtures, one
  nesting `package-set-layout` under the jsoo fixture; RP's middle
  cases run in `_build/host` only because an earlier case replaced the
  contexts.

#### Backend-fixture conversion candidates

Use explicit stable matrices, not blanket native/opaque parity. Keep
requester roles, cold/warm controls and backend-specific assertions.
No helper options or generic runner are planned. Ranked static audit:

- [ ] BM3 `lockdir-workspace-deps/compile-against-workspace-lib.t`:
  common leaf payload through both backends; retain the opaque
  requester, workspace library, transitive linking, section checks and
  edit/backend-switch invalidation. No scenario deletion established.
- [ ] BM4 remaining installed-file cases: `pkg-pform/installed.t`
  overlaps the positive control in `installed-missing-file.t`, but both
  are unchanged on main; deleting one would enlarge this stack's diff.
  Retain external-prefix/OCAMLPATH discovery, rename/error/dependency
  assertions.
  Consider managed providers for the all-section pkg pforms in
  `artifacts.t`, without replacing external-prefix controls. Directory
  pforms differ by backend (directory entries versus installed leaves);
  do not impose parity when sharing producer fixtures.
- [ ] BM5 `dep-section-variables.t`: provider backend x section x
  system/run expansion with opaque requesters. Keep relative/absolute
  paths, lib/libexec, direct installs, pkg-self and collision controls.
  Share payload construction, not the distinct scenarios.
- [ ] BM6 `bin-narrowing/depopts.t`: backend x depends/depopts; preserve
  the hard-dependency control and guarded optional execution.
- [ ] BM7 `bin-narrowing/{mixed-transitive-deps,transitive-deps}.t`:
  vary a locked leaf, retain boundary direction, opaque requesters and
  outside-closure non-build checks. Mixed-chain prose contradicts its
  recorded true result; do not change semantics from the prose.
- Deferred: CLI companion PATH (`exec-dependency-binaries.t`,
  `bin-narrowing/dune-exec.t`) needs the environment/discovery contract.
  E2E explicit/autolock tests need a repository fixture selector: their
  Dune tarballs are already native; source(copy) would bypass solving
  and fetching. Remove neither E2E scenario.

### S20. Test coverage improvements (remaining items unclaimed)

Follow-ups from printing the diagnostics (C2), each a task of its own:
- C2.b DP, SP: a load error reached through a consumer (`(deps (package
  broken))`, `(libraries broken)`, a `%{pkg:..}` pform) prints the file
  location but no "required by" chain. S7 requires the chain.

Kept on purpose, do not remove:
- SP's `cat _source/...` checks: a patch applied through a
  hard-linked or symlinked copy could modify the origin, so they can fail.
- DP's `(alias runtest) (enabled_if %{bin-available:..})` rule:
  its `enabled_if` is evaluated at rule generation and is the trigger the
  case is about.

Tests first, in their own commits; a test that exposes a bug records the
current output and a plan item, it does not fix the bug.

New cases:
- [ ] N11 mounted coverage for mdx, cinaps, utop and melange (one small
  case each, only where `src/` changed for it). rocq is covered by
  `pkg/mounted-rocqproject.t`.

### S21. Fifth simplification audit (2026-11-17, unclaimed unless marked)

Thirteen read-only readers over the stack's diff against `5a68b191`.
Nothing here removes a behaviour or the only assertion of one. Nothing
was built or run: every item needs `@check @fmt` and its named tests.
Line numbers are those of tip `srsutqlx` and will drift; find the site
by the names given. Items already in S17-S20 or the audit notes are not
repeated. Savings are estimates in lines; "P" marks an item that changes
expected output and so needs Ali before promotion. Totals: about 250
source lines and 430 test lines.

Order within a group is by value. One item is one commit unless said.

Measured and not worth it, do not redo (2026-11-17): C7 is not
behaviour-preserving (a broken-symlink `dune-project` or `*.opam` in a
subdirectory makes the shared `gen_load` reader raise
`Option.value_exn`, where today the user gets an `open` error); C11
saves 2 lines, C15 2 against main; C12 (+9), C16 (+9, `files` is main's
code), C17 (+1..+3) and C14's `package_prefixes` guard (+2, main's
wrapper) grow the diff; C14's Install_action bullet would revert
`uvymruxx` (-7). C20's other bullets: `create_unvalidated` is main's
code; the package_layout default needs a helper costing what it saves.
C6 nets +1: `prepare` keeps the pair match unless `Preparation` is
reshaped.

#### Source: take with, or after, another item

- [ ] C22 with S7's "remove `package_errors`" step: `scope.ml`
  `owner_db`'s `all` repeats the case analysis of `mounted_libs`, and
  `Findlib.all_packages findlib >>| List.map ~f:Entry.name` is written
  twice (-8). The two differ today only in the stored-error case.
- [ ] C23 with S17's `%{lib-available}` item (`Resolve_result.invalid`):
  also restore main's fold in `Lib.DB.find`, keeping only A10's
  `src_dir` conjunct (-11 on top of that item's -32).
- [ ] C24 with A9: `package_scopes ctx candidates` is rebuilt by every
  `(context, owner)` partition and every `package_db` call, and
  `owner_libs owner` once per project of a partition: about N*P router
  databases for N requesters. Make it one node per context. Decide
  first whether a mount partition passing `owner_libs None` to
  `Rocq_scope.make` is intended.
- [ ] C26 after the T2 decision: `layout_env` converts `Env.t` to a
  value-list map and back for every dependency, and the conversion is
  written twice (`pkg_rules.ml`). Fold over the value-list map and
  convert once. Tests: `pkg/exported-env.t`,
  `pkg/default-exported-env.t`, `pkg/withenv.t`.
- [ ] C27 in S10 step 5 (legacy path deleted): `Expander.artifacts` and
  `Expander.depends` are `_ Memo.t` only for the legacy expander, both
  stack constructors wrap plain maps in `Memo.return`; make them plain
  and delete the `expand_action` alias (-8).
- [ ] C28 if S17.D3 is not taken: `Fs_memo.canonical_path` (39 lines)
  re-implements `Path.Expert.try_localize_external` (-30). Low
  confidence: main's helper localises a sibling `/ws-foo` of root `/ws`
  and may raise on a non-normalised path; check what
  `Candidate.source_root` can produce. Tests:
  `pkg/local-source-relative-watch.t`, `watching/`.
- [ ] C31 at D12.a: `Loaded_project.build_context` exists for the
  compile-commands hunk only; delete it and store `build_dir` (-4).
- Not worth a commit alone (only if a further caller appears): a
  `Path.t` form of `Pkg_sources.source_location` for the three
  hand-written matches in `merlin.ml`, `ml_sources.ml`, `lib_info.ml`;
  building workspace package variables in `package_layout.ml` with
  `Lock_dir.Pkg_info.variables` (S11 rewrites that function).

#### Tests: independent, no expected output changes


#### Tests: change expected output or drop a case (P; Ali)

For each, the surviving assertion is named. Check S19's rule first.

- [ ] V5 (-14) `pkg/bin-narrowing/mixed-transitive-deps.t`: block 2
  ("declare directly") was the contrast to block 1 on main; both now
  print `true` and the file says so. Its fact is asserted by
  `transitive-deps.t` (`direct-out`) and `lockdir-deps.t`. Delete it
  and fix the header. Decide with BM7. Enlarges the diff against main
  by about 8 lines.
- [ ] V7 (-11, after T19) `pkg/portable-lockdirs/
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
- stored load errors are turned into `Invalid` resolve results.
Owner-specific opaque eligibility and provider dependency scopes remain
necessary: scanning an opaque package builds it. Do not extend the native
public-library rule into unrestricted opaque forcing.
Keep `Findlib.lookup_and_load.resolve_package`: package-specific DBs
may have an explicit owner with `lock_dir_active = false`;
`pkg/package-library-closure.t` fails if that owner is discarded.

### A10. `Lib` closure checks for several providers of one name

`lib.ml` adds a by-name overlap check in the closure, a
`package_scopes` table on every database, and a `src_dir` comparison in
`find`. Two mounts can no longer carry the same public name (each
mount is masked to its own package), but opaque packages still can:
installed findlib names are arbitrary
(`pkg/package-library-closure.t` has a workspace and an opaque provider
of one name). So the checks stay; no test exercises the by-name overlap
or the ppx key's `opaque_packages`.

### A11. Audit new partial functions (planning agent; both parts read)

Every added site was read; the reachable ones are fixed with tests.
Unsure, to read: `Build_system.directory_target_contents_opt` raises a
code error when its directory is not under a directory target (callers
in `pkg_sources.ml`, `source_tree.ml`); `find_loaded_project` for a
directory in `_build/_private` or `_build/_fetch` now raises "invalid
context" where main fell back to the root project (about 40 callers
unaudited).
- Unsure: `pkg_sources.ml:72-73` with `(pkg enabled)`
  and an opam context (main has the same error, but `Scope.DB.create`
  now reaches it for every context).

### L. Loading and the project model (read by an agent)

Savings are changed lines under `src/`. Nothing was built. The agent
marked each item verified-by-reading or plausible; that is kept here.

- L3 (closed, do not revert). `ml_sources.ml` threads a `~source_dir`
  callback through about ten functions for message text only. Reverting
  would print `.pkg/n/d`; S16 decided on the full build path.
- L8 (closed). Trap: the dune-project existence check stays guarded
  inside its body.
- L9 (closed). Traps: `Loaded_project.source_tree_dir` and
  `Dune_load.source_tree_dir` are not duplicates (the latter returns
  `None` for an invalid context and loads one package).

Checked and found necessary: `Package_name.file`; the logical/backing pair in
`Source_tree_file`; `Dune_load.source_tree_dir`; `bin/alias.ml` and
`bin/target.ml`.

### P. Package rules (read by an agent)

Line numbers are the agent's for the working copy at the time. Nothing
was compiled. Savings are changed lines under `src/`.

- P5. `Expander.path` field (~22). Every construction passes
  `Env_path.path (Global.env ())` except `dep_conf_eval.ml:450`, where it
  only affects `%{make}` inside an exported-env value. Decision: that one
  lookup would use dune's PATH instead of the host context's.
- P10 (closed, do not retry). Restoring the dead
  `Lock_dir.transitive_dependency_closure` and
  `Packages.choose_pkg_for_platform` at main's positions was tried and
  dropped: fiddly for no behaviour.

Refinements to existing items:
- S10: the binaries map is built twice (the env chain is S17.A3).
- S11: `package_layout.ml:84-164` re-implements
  `package_universe.ml:127-163` and `Candidate.dependencies`.

Checked and found necessary: the `Log.info` calls a test asserts;
`Dependency.export_paths`; `Install.Entry.Expanded.make`; native branches
in `build_env_of_deps` and `Pkg.dependencies` (a dev tool can share a
natively built dependency); the `install_paths` override; the
per-package `package_entries` memo; `Package_universe.
immediate_dependencies`.

### F. Feature rules (read by an agent)

Nothing was built. Savings are changed lines under `src/`.

- F8 (refines A9). Per-package scopes cost about 60 lines in the feature
  rules: `Merlin.make ~scope ~expander`, `library_rules ~expander`, and
  `for_package` calls in `exe_rules.ml`, `lib_rules.ml`,
  `melange_rules.ml` exist only because a stanza's scope can differ from
  its directory's. Preserve the accepted opaque/provider scope duties
  (A9, B1) when re-auditing this plumbing.

Checked and found necessary: `Normal.combine_exn` map unions;
`simple_rules` `only_sources` branch;
the one-line build-path adaptations in `cram_rules`,
`bootstrap_info`, `revdep_rules`, `ocaml_index`, odoc;
`directory_target_contents_opt` (three callers).

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
- T2 (V). `pkg/default-exported-env.t`: `MANPATH`, `OCAMLPATH`,
  `CAML_LD_LIBRARY_PATH` and `OCAMLTOP_INCLUDE_PATH` end in `:`. Cause
  (by reading): the mounted executor's env is `Pkg_rules.layout_env
  ~base:(Global.env ())`, which joins every install path variable with
  the ambient value through `Install.Roots.extend_env_concat_path_vars`
  (unchanged from main). Main's legacy executor used
  `Value_list_env.extend_concat_path`: only PATH was joined, the other
  variables replaced the ambient value. So ambient `OCAMLPATH` etc. are
  now appended, and the test's set-but-empty ambient value becomes an
  empty entry (`Bin.cons_path` with `Some ""`, a quirk main pins with a
  CR in `src/install/roots.ml`). Decision (Ali): keep the ambient
  values (then fix `Bin.cons_path` on main), or join PATH only as main.
  Same cause as `opam-package-with-setenv.t` above.

Tests tied to machinery S7 will remove:
- T7. `test/expect-tests/memo/run_with_error_handler.ml:74-202` tests
  `Memo.collect_user_errors`; goes with S7 (130 lines).
- T8. `pkg/mounted-dune-package.t:124-221` and
  `pkg/mounted-source-preparation.t:294-310` assert stored load errors
  (`%{lib-available:broken}` is `false` for a package that failed to
  load; "invalidates the stored failure"). Replace with S7's tests.

Implementation details that will churn:
- T11. `pkg/mounted-source-preparation.t:75-83` (log messages "evaluated
  once") and `:312-335` (allocated-words ratio over 8192 files).
- T12. Counts of `.packages`, `_fetch` and `.ppx` entries and
  `_private/default/.lock` paths in `mounted-opam-sources.t`,
  `mounted-dune-package.t`, `mounted-projects.t`, `mounted-rule-parity.t`.
- T14. `test/expect-tests/scope/` includes a `Weak` /
  `Gc.full_major` check for obsolete-result retention, not established
  by blackbox metadata tests. Apply D12.b's source/test-pair audit; keep
  that assertion or equivalent coverage while retaining the implementation.

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
- T21 (V). `pkg/bin-narrowing/ambient-path-fallback.t:168-180` expects
  `%{bin:mybin}` = "from system" but `(system mybin)` = "from lockdir"
  because an earlier command happened to build the provider: an
  undeclared dependency on build state. `resolve-program.t:86-127`
  extends T1 to stanza tools (unscoped `(ocamllex)`/`(menhir)` no longer
  use lock-directory tools).
- T22 (remainder). SL's type-error case expects
  `File ".pkg/foo/bad/bad.ml"`, which cannot be opened from the
  workspace root; goes with S16.F4.
- T24. jsoo. DP 540-542 is the only line behind `Config.compiler` and
  the `~compiler` threading in `jsoo_rules.ml` (167 changed lines); DP
  718-730 behind the `build_config@digest` directories; DP 738-747
  justifies nothing. The compiler identity is needed by scoped and
  mounted directories and by per-directory `(env (binaries))`: main
  re-resolves `js_of_ocaml` in each rule's own directory. It stays; move
  DP 545-554 and 650-769 to `jsoo/` (D13).

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
  PR 360-378; OS 51-72;
  `foreign-stubs/compile-commands/namespace.t` (V: tests a
  `.compile_commands` directory; the internal one is `.dune`).

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
- B2: retain the mounted `Pkg.t` seam while ordinary consumers use it,
  and the adapters needed for dev-tool sharing of native dependencies.
  Inventory `find_package`, installed-file/dependency queries,
  `of_closure`, toolchain and install-alias callers before removal.
  - B2.3 waits for S10's seam: preserve workspace-dependency handling and
    `DB.Pkg_table.project_entries`, including its use to skip project
    digests under the private `.pkg` root and the index-evaluation test.
  - B2.5 is P5's PATH-policy decision, not a mechanical field deletion.

## Validation and submission

- [ ] Record the exact final code revision and main base, commands run,
  focused/full checks, exclusions and platform gaps. Historical passes
  (including the recorded post-rebase run) do not certify a later tip;
  recover their exact revisions from history rather than carrying forward
  a date-only green-suite claim.
- [ ] With Ali's approval, serialize one final
  `./dune.exe build @check @fmt @runtest` on that code revision.
  Documentation-only plan edits need `@check @fmt`, not the full suite.
- [ ] Confirm the behavioural gates in S7, plus dev-tool
  sharing/isolation and host/target regressions after S10/S11.
- [ ] Validate external package backings on macOS/FSEvents and Windows,
  including relative local URLs and absolute build-directory aliases.
- [ ] S14's durable Menhir overlay (its "Submission gate" item).

`bin-pform/windows-install-names.t` and `wasmoo/build-info.t` are outside
the default alias here; earlier checks found the same failures on main.
Reconfirm and report exclusions for the submitted revision, not as passing
coverage.

Report source size against main separately from tests and this plan:
it is the stack's measure (Direction), after the gates above pass.
Keep check logs and historical metrics
in commit messages. Follow the PR template and remove `PLAN.md` before
final submission.

Note in the PR: empty-provider PPX driver hashes change (one key
encoding); `(package (dir ../x))` escaping its project is now rejected
when the project is decoded (main only rejects a directory outside the
workspace).
