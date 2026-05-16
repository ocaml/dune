# Per-module library dependency narrowing

This document describes the algorithm Dune uses to narrow the
library-file dependency set of each compile rule on a per-module basis.
The narrowing was introduced in #14492 (split into PRs #14513–#14521,
referred to internally as layers L1..L9). It supersedes the prior
behaviour in which every compile rule depended on the full glob of
every interface file of every library in the compilation context's
dependency closure.

## Motivation

In the pre-narrowing world, a single compile rule for module `M` in
some stanza depended on the glob of every `.cmi` (and, conditionally,
`.cmx`) of every library reachable through `requires`. The glob is
correct — recompiling `M` must see fresh interfaces — but it is
maximally broad. A change to any interface in any reachable library
invalidated `M`'s compile rule, even when `M` does not mention the module
whose interface changed.

For incremental builds in code bases with many libraries and broad
`requires` graphs, this over-invalidation produced large, avoidable
recompilation cascades. The narrowing computes, for each compile rule,
a subset of those files that the consumer module can actually be
affected by — by inspecting what its `ocamldep` says it references and
walking the cross-library reference graph until convergence. Files
outside that subset are dropped from the rule's dependency set; their
unrelated edits no longer trigger a recompile.

The narrowing must be *sound*: every file the compiler can read while
processing `M` must remain in the dependency set, or builds will be
non-deterministic and incremental builds will silently use stale
artefacts. The algorithm therefore conservatively widens to the
original glob in a small set of edge cases described under "Soundness
recovery" below.

## The algorithm at a glance

For each compile rule for a module `M` in compilation context `cctx`,
parameterised by the compile artefact kind (`Lib_mode.Cm_kind.t` — one
of `Ocaml Cmi`, `Ocaml Cmo`, `Ocaml Cmx`, `Melange Cmi`, `Melange Cmj`),
the source-language kind (`Ml_kind.t` — `Impl` or `Intf`), and the mode
(`Lib_mode.t`):

1. If a small set of preconditions fails (`can_filter = false`), fall
   back to the cctx-wide glob — the same dep set every compile rule
   would have had prior to this work. This avoids needing soundness
   arguments for module kinds that the narrowing was not designed for,
   and lets `Melange` paths pass through unchanged.

2. Otherwise, if the consumer cctx already carries a virtual library
   in `requires` (`has_virtual_impl = true`), fall back to the
   cctx-wide glob. Virtual-impl deps require analysis the BFS does not
   currently do.

3. Otherwise, run the narrowing:
   1. Read the consumer module's own `ocamldep` raw references (impl
      and intf), and those of every module in `M`'s within-cctx
      transitive `dep_graph`.
   2. Union them with the `-open` flag modules. The result is
      `referenced : Module_name.Set.t`.
   3. Use the cctx's `Lib_index` (built once per cctx) to partition
      the cctx's library closure into libraries whose entry modules
      appear in `referenced` (`tight`) and libraries whose `None`-
      entries appear in `referenced` (`non_tight`).
   4. Compute the `Lib.closure` of `tight ∪ non_tight ∪
      pps_runtime_libs` to expand the **lib set** over the library
      `requires` DAG, picking up libs reached only via transparent
      aliases that ocamldep cannot see.
   5. Run a BFS over the **module-reference graph**: starting from
      `referenced`, look up `(lib, entry)` pairs in the lib index
      and read each entry's `ocamldep` raw refs, extending the
      frontier until it converges. The fixpoint `tight_set` is the
      set of *module names* (not libs) transitively reachable.
   6. Re-run `Lib_index.filter_libs_with_modules` with `tight_set` in
      place of `referenced`, producing a fresh `(tight_modules,
      non_tight_set)` partition over the BFS-expanded name set.
   7. Compute `must_glob_set` for the libraries that must glob for
      soundness (wrapped-library wrappers reached transparently, and
      `ppx_runtime_libraries` whose modules are post-pp invisible).
   8. Fold over the closed lib set, classifying each lib into one of
      four buckets and emitting either specific-file deps (for tight
      libs) or a full glob (for non-tight libs).
   9. Compute the include flags scoped to the `kept_libs` set —
      libraries that contributed at least one dep — so that `-I`/`-H`
      reflect only what the consumer can actually reach.

The output is `(include_args, dep_set)`, which the compile rule
consumes via `Action_builder.dyn_deps`.

## The `can_filter` precondition

The entry point for the narrowing is
`Module_compilation.lib_deps_for_module`
(`src/dune_rules/module_compilation.ml`). Before any narrowing runs,
the function computes a `can_filter` boolean conjunction:

- The compile is `Ocaml _` (Melange has its own cm-kind story and is
  not narrowed).
- The supplied `dep_graph` is the real one for this cctx (its `dir`
  equals the cctx's `obj_dir`), not a `Dep_graph.dummy` — synthesised
  / link-time-generated / alias / root modules have no usable
  trans-deps and short-circuit.
- The `dep_graph` contains `M`. Modules synthesised outside the stanza
  (e.g. those handed to `ocamlc_i` for `-i` extraction in unrelated
  flows) are also rejected here.
- `M`'s `Module.kind` is filterable: not `Root`, `Wrapped_compat`,
  `Impl_vmodule`, `Virtual`, or `Parameter`. These kinds have bespoke
  dep stories handled elsewhere (`Dep_rules`, `Virtual_rules`).
- `M` has a source file of the requested `ml_kind` (`Impl` or `Intf`).
- The consumer cctx is itself not a `Virtual_rules.is_virtual_or_
  parameter` cctx. Consumer-side virtual-impl behaviour is owned by
  `Dep_rules`.

If any condition fails, the function returns

```ocaml
(cctx_includes_for_cm_kind (),
 Lib_file_deps.deps_of_entries ~opaque ~cm_kind libs)
```

... the cctx-wide glob, exactly equivalent to the prior behaviour. The
narrowing applies only when all conditions hold.

## The `has_virtual_impl` early-out

When `can_filter` holds, the next check is

```ocaml
let* has_virtual_impl =
  Resolve.Memo.read (Compilation_context.has_virtual_impl cctx)
in
if has_virtual_impl then
  Action_builder.return
    (cctx_includes_for_cm_kind (),
     Lib_file_deps.deps_of_entries ~opaque ~cm_kind libs)
else ...
```

`has_virtual_impl` is true when any library in the cctx's
`requires_compile ∪ requires_hidden` is itself a virtual-library
implementation. The BFS does not currently analyse virtual impls'
contribution to the cctx's namespace; for soundness, all such cctxs
fall back to the cctx-wide glob. The decision is cctx-level, computed
once at `Compilation_context.create` and stored as a
`bool Resolve.t Memo.Lazy.t`.

## The narrowing pipeline

When `can_filter` holds and `has_virtual_impl` is false, the function
proceeds to the narrowing pipeline.

### Step 1: read ocamldep raw refs for consumer + trans deps

```ocaml
let* trans_deps = Dep_graph.deps_of dep_graph m in
let need_impl_deps_of dep_m ~is_consumer =
  if is_consumer
  then (match ml_kind with Impl -> true | Intf -> false)
  else
    (not (Module.has dep_m ~ml_kind:Intf))
    ||
    match cm_kind with
    | Ocaml Cmx -> not opaque
    | Ocaml (Cmi | Cmo) | Melange _ -> false
in
```

The function reads `M`'s impl + intf raw refs, plus the impl + intf
raw refs of every module in `M`'s transitive within-stanza
dependencies. The selectivity table (which `.ml`-side refs to read)
matches the compiler's own conditional behaviour:

| `dep_m` is              | `cm_kind`   | `opaque` | read `.ml`?  |
| ----------------------- | ----------- | -------- | ------------ |
| consumer (`M` itself)   | any         | any      | iff `Impl`   |
| trans_dep, no `.mli`    | any         | any      | yes          |
| trans_dep, has `.mli`   | `Cmx`       | false    | yes (inline) |
| trans_dep, has `.mli`   | `Cmx`       | true     | no           |
| trans_dep, has `.mli`   | `Cmi`/`Cmo` | any      | no           |

Each ocamldep raw-ref read is wrapped through
`Compilation_context.cached_raw_refs` (L8), keyed on
`Raw_refs.Key.t = Consumer { obj_name; ml_kind } | Transitive
{ obj_name; cm_kind }`. The cache short-circuits before allocating the
underlying `Action_builder.t`, so two siblings sharing a transitive
dep only construct the dependency-reading builder once per cctx per
key.

The intf side is always read; the impl side is gated by
`need_impl_deps_of`. The union of impl+intf for each module is the
module's *raw references* — every module name that ocamldep saw it
mention.

### Step 2: compute `referenced`

```ocaml
let* m_raw = read_dep_m_raw m ~is_consumer:true in
let* trans_raw =
  union_module_name_sets_mapped trans_deps
    ~f:(read_dep_m_raw ~is_consumer:false)
in
let all_raw = Module_name.Set.union m_raw trans_raw in
let* flags = Ocaml_flags.get (Compilation_context.flags cctx) mode in
let open_modules = Ocaml_flags.extract_open_module_names flags in
let referenced = Module_name.Set.union all_raw open_modules in
```

`open_modules` are module names brought into scope by `-open` flags
that ocamldep does not see (because they are command-line, not source
syntax). Without them the consumer can mention a module by short name
that the raw refs would miss, opening a soundness gap. Their union
with `all_raw` produces `referenced` — the set of module names this
compile of `M` can possibly resolve.

### Step 3: first lib classification

```ocaml
let { Lib_file_deps.Lib_index.tight; non_tight } =
  Lib_file_deps.Lib_index.filter_libs_with_modules
    lib_index
    ~referenced_modules:referenced
in
```

The cctx-level `Lib_index` (built once at `cctx` creation by
`Compilation_context.build_lib_index`) is a structure indexing each
library's entry modules with each module name. `filter_libs_with_
modules` walks `referenced` against the index and partitions:

- `tight : Module.t list Lib.Map.t` — libraries whose `Some`-entry
  modules appear in `referenced`. The mapped list is the specific
  set of entries referenced. These are candidates for per-module
  deps (`deps_of_entry_modules`).
- `non_tight : Lib.Set.t` — libraries whose `None`-entry modules
  appear (wrapped locals, externals, or unwrapped locals with some
  staged-pps / instrumentation-only entries). These must glob.

A library with mixed `Some`/`None` entries can appear in both — the
caller globs it from the `non_tight` side, since the `None` entries
require glob coverage.

### Step 4: include `pps_runtime_libs` and compute `Lib.closure`

```ocaml
let* pps_runtime_libs =
  Resolve.Memo.read (Compilation_context.pps_runtime_libs cctx)
in
let direct_libs =
  List.sort_uniq ~compare:Lib.compare
    (Lib.Map.keys tight @ Lib.Set.to_list non_tight @ pps_runtime_libs)
in
let* all_libs =
  Resolve.Memo.read (Lib.closure direct_libs ~linking:false ~for_)
in
```

`pps_runtime_libs` introduce module references through post-pp source
that ocamldep cannot see — they are folded into the closure input so
the classification fold sees them. `sort_uniq` canonicalises the
input list because `Lib.closure`'s memo key (L9) is order- and
multiplicity-sensitive.

`Lib.closure` adds libraries reachable through transparent aliases
(re-exports) that ocamldep does not surface — for instance, when a
library `A` re-exports modules from `B` via an alias, references to
`B.M` in source compile to references to `A.M` and ocamldep records
only `A`. The closure pulls `B` in.

### Step 5: compute `must_glob_set`

```ocaml
let wrapped_referenced =
  Lib_file_deps.Lib_index.wrapped_libs_referenced
    lib_index ~referenced_modules:referenced
in
let* must_glob_libs =
  Resolve.Memo.read
    (Lib.closure
       (List.sort_uniq ~compare:Lib.compare
          (Lib.Set.to_list wrapped_referenced @ pps_runtime_libs))
       ~linking:false ~for_)
in
let must_glob_set = Lib.Set.of_list must_glob_libs in
```

Two classes of libraries must always glob even when their entry
modules appear in `tight`:

- **Wrapped library wrappers reached transparently.** When the
  consumer mentions `Lib.M`, the source-level reference is to the
  wrapper module `Lib`, but the compiled output of `M` lives at
  `lib__M`. The cross-library walker reads ocamldep on the wrapper
  module, which only surfaces references to the wrapper's direct
  children. A consumer that reaches `Lib`'s wrapper can reach any
  module of `Lib` via Foo's `(open ...)` or `Lib.Bar.x` syntax; the
  walker cannot enumerate those at narrowing time. Solution: glob
  `Lib.closure(wrapped_referenced)` — every library transitively
  reachable from any referenced wrapper.

- **`ppx_runtime_libraries`.** ppx-rewritten output may reference
  modules from a runtime library that the consumer never names in
  its source. ocamldep on the pre-pp source does not see those
  references. Glob the closure to cover.

The union `Lib.Set.of_list must_glob_libs` is the *must-glob set*;
every member is forced onto the glob path regardless of its entry
classification.

### Step 6: BFS to fixpoint

```ocaml
let* tight_set =
  cross_lib_tight_set
    ~sandbox ~sctx ~lib_index ~initial_refs:referenced
in
```

`cross_lib_tight_set` runs a BFS over the cross-library reference
graph:

```ocaml
let rec loop ~seen ~frontier =
  if Module_name.Set.is_empty frontier then return seen
  else
    let pairs =
      Module_name.Set.fold frontier ~init:[] ~f:(fun name acc ->
        Lib_file_deps.Lib_index.lookup_tight_entries lib_index name @ acc)
    in
    let* discovered =
      union_module_name_sets_mapped pairs ~f:read_entry_deps
    in
    let seen = Module_name.Set.union seen frontier in
    let frontier = Module_name.Set.diff discovered seen in
    loop ~seen ~frontier
```

Each frontier iteration:

1. For each module name in the frontier, look up the `(lib, entry)`
   tight-eligible pairs via `Lib_file_deps.Lib_index.lookup_tight_
   entries`. The lookup skips entries with `None`-module (wrapped
   locals, externals) — those are non-tight-eligible and terminate
   chains through them.
2. For each `(lib, entry)`, read the entry module's impl + intf
   ocamldep raw refs.
3. Union all the discovered names into `discovered`.
4. Add the current frontier to `seen`; the new frontier is
   `discovered \ seen`.

The post-pp module map (built by `Compilation_context.build_lib_
index`) ensures that the entry module passed to ocamldep is the form
the dep lib's own compile pipeline produces — so the walker reads
either the `.pp.ml` (for action-style preprocessors or non-staged
pps), or the source directly (for no-preprocessing and future-syntax
extensions, which Dune handles at parse time).

The walker terminates because the universe of module names is
finite (bounded by the cctx's transitive library set) and `seen`
only grows. The cost is proportional to the number of distinct
entry modules transitively reachable from `referenced`.

### Step 7: second lib classification

```ocaml
let { Lib_file_deps.Lib_index.tight = tight_modules;
      non_tight = non_tight_set } =
  Lib_file_deps.Lib_index.filter_libs_with_modules
    lib_index ~referenced_modules:tight_set
in
```

Re-partition the index using the converged `tight_set` (which is a
superset of `referenced`). This is a separate classification from
step 3 because the BFS may have brought new module names into scope
that flip a library from "unreached" to "tight" or change the
specific entries referenced.

### Step 8: classify and emit per-lib deps

```ocaml
let tight_deps, glob_libs, kept_libs =
  List.fold_left all_libs ~init:(Dep.Set.empty, [], Lib.Set.empty)
    ~f:(fun (td, gl, kl) lib ->
      if Lib.Set.mem must_glob_set lib || Lib.Set.mem non_tight_set lib
      then td, lib :: gl, Lib.Set.add kl lib
      else (
        match Lib.Map.find tight_modules lib with
        | Some modules ->
          ( Dep.Set.union td
              (Lib_file_deps.deps_of_entry_modules
                 ~opaque ~cm_kind lib modules)
          , gl
          , Lib.Set.add kl lib )
        | None ->
          if Lib_file_deps.Lib_index.is_tight_eligible lib_index lib
          then td, gl, kl
          else td, lib :: gl, Lib.Set.add kl lib))
in
let glob_deps = Lib_file_deps.deps_of_entries ~opaque ~cm_kind glob_libs in
```

Each library in `all_libs` (the closure-expanded set) is bucketed:

- **Must-glob**: lib is in `must_glob_set` (wrapped-referenced or
  pps-runtime), or has a `None`-entry referenced → emit the full lib
  glob via `deps_of_entries`. Added to `kept_libs`.

- **Tight**: lib has a `Some`-entry referenced and is not must-glob →
  emit `deps_of_entry_modules` for just those referenced entries.
  Added to `kept_libs`.

- **Unreached + tight-eligible**: lib has no referenced entries and is
  tight-eligible (has at least one `Some`-entry) → drop entirely. The
  link rule still pulls it in via `requires_link` for runtime
  linkage. The compile rule does not need it.

- **Unreached + not tight-eligible**: lib has no referenced entries
  but is not tight-eligible (wrapped local, external) → emit glob.
  Added to `kept_libs`.

The `kept_libs` accumulator captures every library that contributes
*any* dep to this compile rule. It is the precise set against which
include flags should be scoped (step 9).

### Step 9: filtered include flags

```ocaml
let+ include_flags =
  Compilation_context.filtered_include_flags cctx ~cm_kind ~kept_libs
in
include_flags, Dep.Set.union tight_deps glob_deps
```

`Compilation_context.filtered_include_flags` (L6+L7) restricts the
cctx's `requires_compile` and `requires_hidden` lists to libraries in
`kept_libs`, then computes `-I` / `-H` over those subsets. The result
is memoised per `(lib_mode, kept_libs : Lib.Set.t)` (L7) so that two
modules with the same `kept_libs` share evaluation.

The final return value is the include args paired with the total dep
set. `Module_compilation.lib_cm_deps` wraps the function with
`Action_builder.dyn_deps`, which registers `dep_set` as the rule's
dynamic deps and returns just the include args for the command line.

## Data structures

### `Lib_file_deps.Lib_index.t`

(`src/dune_rules/lib_file_deps.{ml,mli}`.) Built once per cctx by
`Compilation_context.build_lib_index ~super_context ~libs ~for_`. The
input is a list of `(Module_name.t, Lib.t, Module.t option)` triples:
the entry module's name, the owning library, and the entry's
`Module.t` if available (`None` for wrapped locals and externals).

The index supports:

- `filter_libs_with_modules ~referenced_modules` — partition into
  tight (per-lib `Module.t list` for the referenced entries) and
  non_tight (libs with `None`-entry references).
- `lookup_tight_entries name` — the `(lib, entry)` pairs whose entry
  name is `name`, skipping `no_ocamldep`-tagged libs and `None`-entry
  rows. Used by the BFS frontier expansion.
- `is_tight_eligible lib` — true iff lib has at least one `Some`-
  entry. Used in step 8 to distinguish "drop" from "must glob".
- `wrapped_libs_referenced ~referenced_modules` — the set of wrapped
  local libraries whose wrapper module name appears. Used to compute
  the wrapped-soundness must-glob input in step 5.

### `Compilation_context.cached_raw_refs` (L8)

A per-cctx `Hashtbl` keyed by `Raw_refs.Key.t` that memoises the
`Module_name.Set.t Action_builder.t` for a given module's ocamldep
raw refs. Two distinct keys:

- `Consumer { obj_name; ml_kind }` — used when reading the consumer
  module `M`'s own refs (`ml_kind` distinguishes `Impl` from `Intf`
  contexts; the choice of what to read varies per
  `need_impl_deps_of`).
- `Transitive { obj_name; cm_kind }` — used when reading a
  `trans_deps` member's refs.

Within a cctx, two compile rules computing narrowing for siblings
that share a transitive dep both hit the same key, paying the
underlying ocamldep read once.

### `Compilation_context.filtered_includes` (L7)

A per-cctx `Hashtbl` keyed by `Filtered_includes.Key.t =
{ lib_mode : Lib_mode.t; kept_libs : Lib.Set.t }`. Caches the include
flags Action_builder per `(lib_mode, kept_libs)` tuple. Two modules
within the same cctx whose narrowing produces identical `kept_libs`
share the include-flag computation.

### `Lib.closure` memo (L9)

`Lib.closure` is wrapped in a memo keyed by `(linking, for_, libs)`
where `libs` is the input lib list. Two narrowing calls within the
same cctx (or across cctxs) that produce identical `direct_libs`
share the closure computation.

### `Dep_graph` per cctx, per `Ml_kind`

(`src/dune_rules/dep_rules.ml`.) Built per cctx as
`Ml_kind.Dict.t Dep_graph.t`. `Dep_graph.deps_of dep_graph m` returns
the within-cctx transitive `Module.t list` for module `m`. Used at
step 1 to enumerate trans_deps before reading their ocamldep raw
refs.

## Soundness recovery and known edge cases

Three categories of fallback to the glob:

1. **Module kinds outside `module_kind_is_filterable`**: `Root`,
   `Wrapped_compat`, `Impl_vmodule`, `Virtual`, `Parameter`. Each has
   a bespoke dep story handled elsewhere; the `can_filter` gate
   rejects them.

2. **Consumer cctx with `has_virtual_impl = true`**: the cctx itself
   has a virtual-impl in `requires`. The BFS currently lacks the
   analysis to safely narrow under that condition; the entire cctx
   falls back.

3. **Per-library wrapped or pps-runtime soundness**: handled within
   the narrowing by `must_glob_set` (step 5 + step 8). The
   classification fold force-globs these libraries even when their
   tight classification would otherwise narrow them.

`Melange` paths bypass the narrowing entirely at the `can_filter`
check; the cm_kind machinery there is different and L9 leaves it
unchanged.

## Cost characteristics

Per consumer module, the narrowing does:

- One ocamldep raw-ref read for the consumer (cached by L8).
- One ocamldep raw-ref read per trans-dep module (cached by L8 —
  the read is shared with sibling consumers that share a trans-dep).
- One `Lib.closure` (memoised by L9 — shared with sibling consumers
  whose `direct_libs` are identical).
- One BFS over the cross-library reference graph (state machine runs
  per module; the individual ocamldep reads inside it hit L8 across
  modules).
- Two `filter_libs_with_modules` calls (step 3 and step 7).
- One `filtered_include_flags` lookup (memoised by L7 — shared with
  siblings whose `kept_libs` match).

The visible per-module Dune-level functions are individually small.
The cost is in their *downstream effects* — `Module_name.Set` unions,
`Lib.Set` / `Lib.Map` operations, `Dep.Set` constructions,
`Action_builder` bind chains, and the minor-GC oldification of the
intermediate sets.

On a null build, the narrowing yields no benefit (no `.cmi` changed,
so no module would have rebuilt either way). The work runs anyway.
The narrowing trades null-build wall time for incremental-build
recompile reduction.

## Layer-by-layer construction (#14492)

For reference, the algorithm above is the cumulative result of:

- **L1** (#14513): test infrastructure prerequisite (merged).
- **L2** (#14514): cctx fields `Lib_index`, `has_virtual_impl`,
  `pps_runtime_libs`.
- **L3** (#14515): scaffold — route lib file deps through a per-
  module function (`lib_deps_for_module`) that initially returns the
  same cctx-wide glob, preparing the call site.
- **L4** (#14516): the BFS itself (`cross_lib_tight_set`,
  `lib_deps_for_module` body); the `can_filter` gate is added.
- **L5** (#14517): soundness recovery —
  `has_virtual_impl` early-out, `pps_runtime_libs` fold-in,
  `must_glob_set` from wrapped-referenced libs.
- **L6** (#14518): `filtered_include_flags` per `kept_libs`.
- **L7** (#14519): `Filtered_includes.t` per-cctx cache keyed by
  `(lib_mode, kept_libs)`.
- **L8** (#14520): `cached_raw_refs` per-cctx cache for ocamldep
  raw-ref `Action_builder.t`s.
- **L9** (#14521): `Lib.closure` memo keyed by `(linking, for_,
  libs)`.

Each layer can be read in isolation in the PR stack.
