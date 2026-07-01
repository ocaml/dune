# OCaml code style

OCaml-style reference for reviewing diffs in this repo. Most rules trace back to
`doc/hacking.rst`; cite the file when invoking a rule, but a one-line nit is
usually enough.

## Module structure

- Every `.ml` needs an `.mli` (except type-only files).

- Module documentation at the top of every `.mli` — purpose and invariants.

- `.mli` exposes only what other modules call. No speculative API; no re-exports
  for grepability.

- **No `.mli`-only modules** — they offer no advantages over `.ml` modules with
  type definitions, and you can't define exceptions in them.

- `snake_case` for all identifiers, including modules and module types.

## Type design

- **Use labelled arguments** when type alone isn't self-descriptive — e.g. `val
  display_name : first_name:string -> last_name:string -> _ Pp.t`.

- **Module `Foo` with type `t`** over a bare top-level type `foo`. Put related
  functions in the module.

- **Share module types via `Foo_intf`** — an `.ml`-only module that defines the
  module type once instead of repeating it across `.ml` and `.mli`.

- **`t` comes first.** If a module defines `type t`, functions in it take `t` as
  the first argument.

- **Precise path types**, not strings: `Path.Local.t`, `Path.Build.t`,
  `Path.Source.t`, `Path.External.t`.

- **No type aliases** unless strongly justified.

- **No polymorphic variants** when a regular variant works.

- An `Id` type should uniquely identify something.

- Don't write `to_dyn` directly. Write `Repr.t` values and derive `to_dyn` from
  them.

## Error handling

- `Code_error.raise` over `assert false`. Describe the broken invariant.

- No `Invalid_argument` — `Code_error.raise` gives richer payloads.

- `User_error` raised with `Pp.text` for user-facing failures.

- When ignoring a binding, use `let (_ : t) = ...` with a type annotation, not
  bare `let _ = ...`. Critical for `Fiber.t` — a bare ignore there is a real
  bug.

## Pattern matching

- Sort match clauses by RHS length, shortest at top.

- Don't qualify constructors/fields when matching. Annotate the scrutinee
  instead: `(args : _ Command.Args.t) | S [] -> ...` rather than `Command.Args.S
  []`.

## Records

- Construct with `{ A.field = ... }`, not `A.{ field = ... }` — the local-open
  form pulls in shadowing names.

- Destructure rather than project: `let { A.field; _ } = record` not
  `record.A.field`.

- When writing `to_dyn` on a record manually, use a positional pattern with
  `Dyn.record`; ignore fields explicitly with `; d = _`, never `; _`.

- **Equality functions follow the same pattern**: positional destructure on the
  LHS, project on the RHS; ignore unused fields with `; d = _`.

## Naming and binding

- Avoid meaningless names: `x`, `a`, `b`, `f`. Inline or rename.

- Avoid optional arguments — they encourage caller laziness and are annoying to
  grep. Pass the argument always when callers are few.

- Local `let` bindings that shadow module-level functions should be renamed.

- Define bindings close to their use site; keep scope small.

- **Name bindings to enable record/label punning.** When a binding will be used
  as a record field or labelled argument, give it the same name.

## Expression idioms

- `if x then true else false` is just `x`.

- `if not x then foo else bar` should be inverted.

- Don't mix `|>` and `@@` in the same expression.

- Prefer `|>` chaining over right-leaning nested calls.

- No "logic-less chain of functions" helpers — if `bar` and `baz` are already
  public, don't add `let foo t = bar t |> baz`.

## Comments

- Default to none. Only add when the *why* is non-obvious — a workaround, a
  non-trivial invariant, a known limitation.

- Don't paste docstrings into both `.mli` and `.ml`.

## `Pp` printing

- Use `Pp.cut` and `Pp.space`. **Never `Pp.newline`** — it breaks the enclosing
  box's layout decisions and makes the value uncomposable.

## Phrasings

When raising a style nit, be terse and Socratic. Common templates:

- "Can you use `Path.Local.t` here?"

- "Can you use some `|>` to avoid the right-leaning indentation style?"

- "I'd move the shorter clauses to the top."

- "Where is this function being used outside of this module?"

- "Could you move this function to the toplevel? This allocation should be
  avoided."

- "If you move `children` to be the first argument, the code will be more
  readable and shorter."
