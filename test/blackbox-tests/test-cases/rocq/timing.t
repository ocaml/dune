Per-file timing (rocq compile -time-file) and profiling (rocq compile
-profile) outputs, enabled through the env stanza.

  $ make_rocq_project 3.26 0.11

  $ cat > foo.v <<EOF
  > Definition foo := 0.
  > EOF

  $ cat > bar.v <<EOF
  > From basic Require Import foo.
  > Definition bar := foo.
  > EOF

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name basic))
  > EOF

  $ list_outputs () {
  >   find _build/default -name '*.timing' -o -name '*.prof.json' | sort
  > }

By default, no timing or profiling outputs are produced:

  $ dune build @all
  $ list_outputs

Enabling them in the workspace:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.26)
  > (env
  >  (_
  >   (rocq
  >    (timing true)
  >    (profiling true))))
  > EOF

  $ dune build @all
  $ list_outputs
  _build/default/bar.v.timing
  _build/default/bar.vo.prof.json
  _build/default/foo.v.timing
  _build/default/foo.vo.prof.json

  $ grep -c '^Chars ' _build/default/bar.v.timing
  2

The outputs are targets, so they are not removed by the next build and can be
requested directly:

  $ dune build @all
  $ list_outputs
  _build/default/bar.v.timing
  _build/default/bar.vo.prof.json
  _build/default/foo.v.timing
  _build/default/foo.vo.prof.json

  $ rm -rf _build
  $ dune build ./foo.v.timing
  $ list_outputs
  _build/default/foo.v.timing
  _build/default/foo.vo.prof.json

A nearer env stanza overrides the workspace one:

  $ cat > dune <<EOF
  > (env
  >  (_
  >   (rocq
  >    (profiling false))))
  > (rocq.theory
  >  (name basic))
  > EOF

  $ dune build @all
  $ list_outputs
  _build/default/bar.v.timing
  _build/default/foo.v.timing

The fields require dune 3.26:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (env
  >  (_
  >   (rocq
  >    (timing true))))
  > EOF

  $ dune build @all
  File "dune-workspace", line 5, characters 3-16:
  5 |    (timing true))))
         ^^^^^^^^^^^^^
  Error: 'timing' is only available since version 3.26 of the dune language.
  Please update your dune-project file to have (lang dune 3.26).
  [1]
