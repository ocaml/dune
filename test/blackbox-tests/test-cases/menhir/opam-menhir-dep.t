When a package's [(depends ...)] field lists [menhir] without a
version constraint, the generated opam file should fill in the
lower bound [{>= "20180523"}], since dune's menhir rules rely on
features available since menhir 20180523.

See https://github.com/ocaml/dune/issues/10707.

Case 0: package does not declare menhir. Dune does not add it.

  $ cat > dune-project << EOF
  > (lang dune 3.24)
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
  > (lang dune 3.24)
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
  > (lang dune 3.24)
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
  > (lang dune 3.24)
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
  > (lang dune 3.24)
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
  > (lang dune 3.24)
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
  > (lang dune 3.24)
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
  Warning: The lower bound >= 20100101 on menhir in the depends field is less
  than the version dune's menhir rules require. The generated opam file will
  use >= 20180523 instead.
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
