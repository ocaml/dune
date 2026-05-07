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
constraint and is preserved verbatim — the lower bound is not
combined with it.

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
    "menhir" {with-test}
