When a package's [(depends ...)] field lists [menhir] without a
version constraint, the generated opam file should fill in the
lower bound [{>= "20180523"}], since dune's menhir rules rely on
features available since menhir 20180523.

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

Case 1: bare [(depends menhir)]. Dune fills in the lower bound.

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

Case 3: gate on [(using menhir ...)]. Without the menhir extension
enabled, dune does not run menhir's rules and so must not impose
the lower bound on a user-declared menhir dependency that exists
for an unrelated reason (e.g. runtime).

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

Case 4: multi-package regression for #14428. Package [foo] declares
[(depends menhir)]; package [bar] declares no menhir dep. The
generated opam files must reflect this: [foo.opam] gets the lower
bound; [bar.opam] has no [menhir] line at all.

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

Case 5: a [{with-test}] filter on the menhir dep is a non-version
constraint; the generated opam file ANDs dune's required lower
bound with it.

Case 4 left a [bar.opam] behind; remove it first, otherwise dune
errors because the new dune-project below has no [bar] package
stanza for the existing opam file.

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

Case 6: user-written lower bound below dune's required minimum.
Dune warns and the generated opam file uses [>= "20180523"]
instead of the user's bound.

Case 5 left [foo.opam] behind; remove it so this case starts
fresh.

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

Skip [--auto-promote]: the generated file lives at
[_build/default/foo.opam.generated] under [(lang dune 3.23)] or
newer, so we can read it without promoting to source. Assert two
things: the warning is emitted with its full text, and the
generated opam file uses dune's bound rather than the user's.

  $ dune build @opam 2>&1 | dune_cmd print-from '^Warning:' | dune_cmd print-until 'instead\.$'
  Warning: A lower bound on menhir in the depends field is less than the
  version dune's menhir rules require. The generated opam file will use >=
  20180523 instead.
  [1]
  $ grep menhir _build/default/foo.opam.generated
    "menhir" {>= "20180523"}

Case 7: lang version below 3.23. Dune does nothing to the menhir
dep constraint; the generated opam file preserves the user's input
verbatim. The whole upgrade step is gated on [(lang dune 3.23)] or
newer to avoid changing opam output on dune-binary upgrade for
projects pinned to an older lang version (parallels #14466 / the
fix for #14436). The gate also covers the bare-[(menhir)] fill
from #14434, which previously applied unversioned.

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

Case 8: lang version below 3.23 with bare [(menhir)]. The 3.23
gate also covers #14434's bare-fill — dune does not inject
[>= "20180523"]; the opam file lists [menhir] without a
constraint. Together with Case 7, this verifies both branches of
[upgrade_menhir_constraint] are gated: the bare branch (from
#14434) and the user-constraint branch (from this PR's merge
logic).

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

Cases 9-13 exercise the merge function's recursive descent into
the user's constraint expression — see also #14453 review by
@shonfeder. The merge walks [And]/[Or]/[Not] looking for [>= "v"]
literals to tighten; pure-filter leaves and [Bop]/[Not] subtrees
pass through unchanged. If, after tightening, every satisfying
valuation of the user expression implies [>= menhir_min_version],
the merge returns the rewritten expression directly; otherwise it
AND's [menhir_constraint] at the top level.

Case 9: nested lower bound below dune's minimum, combined with a
filter via [(and ...)]. The merge descends into the [And],
tightens the inner [>=] (warns), and returns the rewritten
expression directly because the inner bound now suffices.

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

Case 10: nested lower bound at or above the minimum is preserved
verbatim; no outer AND is added (the inner bound suffices, no
warning).

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

Case 11: an [Or] whose branches mix a (low) lower bound with a
filter. The merge tightens the in-branch [>=] (warns); since the
[Or] does not imply the minimum on every branch (the filter-only
branch escapes), the outer AND with dune's bound is still
emitted.

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

Case 12: a user-declared version range whose lower end is below
dune's minimum. The merge tightens the [>=] in place (warns) and
preserves the upper bound. The merged range starts at dune's
minimum.

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

Case 13: a user-declared expression that conflicts with dune's
minimum — here [(and :with-test (< 1))] — has no inner [>=] to
tighten. The merge AND's dune's bound at the top level; the
result is generated faithfully but is unsatisfiable as an opam
constraint. The conflict surfaces at install time (opam reports
no version satisfies the combined constraints) rather than at
opam-file generation time.

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
