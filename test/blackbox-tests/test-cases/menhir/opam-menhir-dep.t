When a package's `(depends ...)` field lists `menhir` without a version
constraint, the generated opam file should fill in the lower bound `{>=
"20180523"}`, since dune's menhir rules rely on features available since menhir
20180523.

See https://github.com/ocaml/dune/issues/10707.

Case 0: package does not declare menhir. Dune does not add it.

  $ cat > dune-project << EOF
  > (lang dune 3.23)
  > (using menhir 2.1)
  > (generate_opam_files true)
  > (package (name foo) (allow_empty))
  > EOF

  $ dune build @opam --auto-promote > /dev/null 2>&1
  [1]
  $ grep menhir foo.opam
  [1]

Case 1: bare `(depends menhir)`. Dune fills in the lower bound.

  $ cat > dune-project << EOF
  > (lang dune 3.23)
  > (using menhir 2.1)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends menhir))
  > EOF

  $ dune build @opam --auto-promote > /dev/null 2>&1
  [1]
  $ grep menhir foo.opam
    "menhir" {>= "20180523"}

Case 2: user-written version bound is preserved verbatim.

  $ cat > dune-project << EOF
  > (lang dune 3.23)
  > (using menhir 2.1)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends (menhir (>= 20211128))))
  > EOF

  $ dune build @opam --auto-promote > /dev/null 2>&1
  [1]
  $ grep menhir foo.opam
    "menhir" {>= "20211128"}

Case 3: gate on `(using menhir ...)`. Without the menhir extension enabled, dune
does not run menhir's rules and so must not impose the lower bound on a
user-declared menhir dependency that exists for an unrelated reason (e.g.
runtime).

  $ cat > dune-project << EOF
  > (lang dune 3.23)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends menhir))
  > EOF

  $ dune build @opam --auto-promote > /dev/null 2>&1
  [1]
  $ grep menhir foo.opam
    "menhir"

Case 4: multi-package regression for #14428. Package `foo` declares `(depends
menhir)`; package `bar` declares no menhir dep. The generated opam files must
reflect this: `foo.opam` gets the lower bound; `bar.opam` has no `menhir` line
at all.

  $ cat > dune-project << EOF
  > (lang dune 3.23)
  > (using menhir 2.1)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends menhir))
  > (package (name bar) (allow_empty))
  > EOF

  $ dune build @opam --auto-promote > /dev/null 2>&1
  [1]
  $ grep menhir foo.opam
    "menhir" {>= "20180523"}
  $ test -f bar.opam && grep -c '^opam-version' bar.opam
  1
  $ grep menhir bar.opam
  [1]

Case 5: a `{with-test}` filter on the menhir dep is a non-version constraint;
the generated opam file ANDs dune's required lower bound with it.

Case 4 left a `bar.opam` behind; remove it first, otherwise dune errors because
the new dune-project below has no `bar` package stanza for the existing opam
file.

  $ rm -f bar.opam
  $ cat > dune-project << EOF
  > (lang dune 3.23)
  > (using menhir 2.1)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends (menhir :with-test)))
  > EOF

  $ dune build @opam --auto-promote > /dev/null 2>&1
  [1]
  $ grep menhir foo.opam
    "menhir" {>= "20180523" & with-test}

Case 6: user-written lower bound below dune's required minimum. Dune warns and
the generated opam file uses `>= "20180523"` instead of the user's bound.

Case 5 left `foo.opam` behind; remove it so this case starts fresh.

  $ rm -rf _build foo.opam
  $ cat > dune-project << EOF
  > (lang dune 3.23)
  > (using menhir 2.1)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends (menhir (>= 20100101))))
  > EOF

Skip `--auto-promote` so the warning lands in stdout; read the unpromoted file
at `_build/default/foo.opam.generated`.

  $ dune build @opam 2>&1 | dune_cmd print-from '^Warning:' | dune_cmd print-until 'instead\.$'
  Warning: A lower bound on menhir in the depends field is less than the
  version dune's menhir rules require. The generated opam file will use >=
  20180523 instead.
  [1]
  $ grep menhir _build/default/foo.opam.generated
    "menhir" {>= "20180523"}

Case 7: with `(lang dune 3.22)`, the menhir constraint is preserved verbatim.
The 3.23 gate avoids changing opam output on dune-binary upgrade for projects
pinned to an older lang version (parallels #14466 / fix for #14436).

  $ rm -rf _build foo.opam
  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > (using menhir 2.1)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends (menhir (>= 20100101))))
  > EOF
  $ dune build @opam --auto-promote > /dev/null 2>&1
  $ grep menhir foo.opam
    "menhir" {>= "20100101"}

Case 8: with `(lang dune 3.22)` and a bare `(menhir)` dep, dune does not inject
`>= "20180523"`. Together with Case 7, this verifies the gate covers both bare
deps (from #14434) and deps with a user-written constraint.

  $ rm -rf _build foo.opam
  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > (using menhir 2.1)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends menhir))
  > EOF
  $ dune build @opam --auto-promote > /dev/null 2>&1
  $ grep menhir foo.opam
    "menhir"

Cases 9-15 cover user-written menhir constraints with nested structure (`(and
...)`, `(or ...)`, `(not ...)`) — the lower bound is enforced even when the
user's `>=` is buried inside the expression.

Case 9: the user's lower bound is nested inside `(and ...)` alongside a filter,
and is below dune's minimum. Dune warns and the generated opam file uses `>=
"20180523"` in place of the user's low bound; the filter is preserved.

  $ rm -rf _build foo.opam
  $ cat > dune-project << EOF
  > (lang dune 3.23)
  > (using menhir 2.1)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends (menhir (and :with-test (>= 20100101)))))
  > EOF
  $ dune build @opam 2>&1 | dune_cmd print-from '^Warning:' | dune_cmd print-until 'instead\.$'
  Warning: A lower bound on menhir in the depends field is less than the
  version dune's menhir rules require. The generated opam file will use >=
  20180523 instead.
  [1]
  $ grep menhir _build/default/foo.opam.generated
    "menhir" {with-test & >= "20180523"}

Case 10: the user's lower bound is nested inside `(and ...)` alongside a filter,
and is at or above dune's minimum. The user's constraint is preserved verbatim;
no warning.

  $ rm -rf _build foo.opam
  $ cat > dune-project << EOF
  > (lang dune 3.23)
  > (using menhir 2.1)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends (menhir (and :with-test (>= 20211128)))))
  > EOF
  $ dune build @opam --auto-promote > /dev/null 2>&1
  [1]
  $ grep menhir foo.opam
    "menhir" {with-test & >= "20211128"}

Case 11: the user offers an `(or ...)` between a low lower bound and a filter.
Dune warns and the generated opam file enforces `>= "20180523"` alongside the
user's expression — the filter-only branch would otherwise admit pre-20180523
versions.

  $ rm -rf _build foo.opam
  $ cat > dune-project << EOF
  > (lang dune 3.23)
  > (using menhir 2.1)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends (menhir (or (>= 20100101) :with-test))))
  > EOF
  $ dune build @opam 2>&1 | dune_cmd print-from '^Warning:' | dune_cmd print-until 'instead\.$'
  Warning: A lower bound on menhir in the depends field is less than the
  version dune's menhir rules require. The generated opam file will use >=
  20180523 instead.
  [1]
  $ grep menhir _build/default/foo.opam.generated
    "menhir" {>= "20180523" & (>= "20180523" | with-test)}

Case 12: the user declares a version range whose lower end is below dune's
minimum. Dune warns; the upper bound is preserved and the lower bound becomes
`>= "20180523"` in the generated opam file.

  $ rm -rf _build foo.opam
  $ cat > dune-project << EOF
  > (lang dune 3.23)
  > (using menhir 2.1)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends (menhir (and (>= 20100101) (< 20300101)))))
  > EOF
  $ dune build @opam 2>&1 | dune_cmd print-from '^Warning:' | dune_cmd print-until 'instead\.$'
  Warning: A lower bound on menhir in the depends field is less than the
  version dune's menhir rules require. The generated opam file will use >=
  20180523 instead.
  [1]
  $ grep menhir _build/default/foo.opam.generated
    "menhir" {>= "20180523" & < "20300101"}

Case 13: the user's expression conflicts with dune's minimum — here `(and
:with-test (< 1))`. The generated opam file enforces `>= "20180523"` alongside
the user's expression; the combination is unsatisfiable, and the conflict
surfaces at install time (opam reports no version satisfies the combined
constraints) rather than at opam-file generation.

  $ rm -rf _build foo.opam
  $ cat > dune-project << EOF
  > (lang dune 3.23)
  > (using menhir 2.1)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends (menhir (and :with-test (< 1)))))
  > EOF
  $ dune build @opam --auto-promote > /dev/null 2>&1
  [1]
  $ grep menhir foo.opam
    "menhir" {>= "20180523" & with-test & < "1"}

Case 14: a user-declared equality pin at or above dune's minimum. The pin
already implies the lower bound, so the generated opam file uses it verbatim.

  $ rm -rf _build foo.opam
  $ cat > dune-project << EOF
  > (lang dune 3.23)
  > (using menhir 2.1)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends (menhir (= 20211128))))
  > EOF
  $ dune build @opam --auto-promote > /dev/null 2>&1
  [1]
  $ grep menhir foo.opam
    "menhir" {= "20211128"}

Case 15: a `(not (>= v))` expression with `v` below dune's minimum. Dune warns
and the generated opam file enforces `>= "20180523"` alongside the user's
expression. The combination is unsatisfiable, as in Case 13.

  $ rm -rf _build foo.opam
  $ cat > dune-project << EOF
  > (lang dune 3.23)
  > (using menhir 2.1)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends (menhir (not (>= 20100101)))))
  > EOF
  $ dune build @opam 2>&1 | dune_cmd print-from '^Warning:' | dune_cmd print-until 'instead\.$'
  Warning: A lower bound on menhir in the depends field is less than the
  version dune's menhir rules require. The generated opam file will use >=
  20180523 instead.
  [1]
  $ grep menhir _build/default/foo.opam.generated
    "menhir" {>= "20180523" & !>= "20180523"}

Case 16: `(not (< v))` with `v` at or above dune's minimum. The expression is
equivalent to `(>= v)`, so it's a sufficient lower bound and is preserved
verbatim; no warning, no extra `>= "20180523"` clause.

  $ rm -rf _build foo.opam
  $ cat > dune-project << EOF
  > (lang dune 3.23)
  > (using menhir 2.1)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends (menhir (not (< 20211128)))))
  > EOF
  $ dune build @opam --auto-promote > /dev/null 2>&1
  [1]
  $ grep menhir foo.opam
    "menhir" {!< "20211128"}

Case 17: `(not (<= v))` with `v` at or above dune's minimum. The expression is
equivalent to `(> v)`, which strictly exceeds `v` and therefore exceeds dune's
minimum; preserved verbatim, no warning.

  $ rm -rf _build foo.opam
  $ cat > dune-project << EOF
  > (lang dune 3.23)
  > (using menhir 2.1)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends (menhir (not (<= 20211128)))))
  > EOF
  $ dune build @opam --auto-promote > /dev/null 2>&1
  [1]
  $ grep menhir foo.opam
    "menhir" {!<= "20211128"}

Case 18: `(not (< v))` with `v` below dune's minimum. The expression is
equivalent to `(>= v)` but `v` is too low to satisfy dune's requirement; the
generated opam file ANDs `>= "20180523"` with the user's expression. No
warning is emitted (the user's clause is not rewritten in place; only `>= v`
literals are rewritten by `normalize_lower_bounds`).

  $ rm -rf _build foo.opam
  $ cat > dune-project << EOF
  > (lang dune 3.23)
  > (using menhir 2.1)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends (menhir (not (< 20100101)))))
  > EOF
  $ dune build @opam --auto-promote > /dev/null 2>&1
  [1]
  $ grep menhir foo.opam
    "menhir" {>= "20180523" & !< "20100101"}
