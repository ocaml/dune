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
  File "dune-workspace", line 5, characters 4-10:
  5 |    (timing true)
          ^^^^^^
  Error: Unknown field "timing"
  [1]
  $ list_outputs

  $ grep -c '^Chars ' _build/default/bar.v.timing
  grep: _build/default/bar.v.timing: No such file or directory
  [2]

The outputs are targets, so they are not removed by the next build and can be
requested directly:

  $ dune build @all
  File "dune-workspace", line 5, characters 4-10:
  5 |    (timing true)
          ^^^^^^
  Error: Unknown field "timing"
  [1]
  $ list_outputs

  $ rm -rf _build
  $ dune build ./foo.v.timing
  File "dune-workspace", line 5, characters 4-10:
  5 |    (timing true)
          ^^^^^^
  Error: Unknown field "timing"
  [1]
  $ list_outputs
  find: '_build/default': No such file or directory
  [1]

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
  File "dune-workspace", line 5, characters 4-10:
  5 |    (timing true)
          ^^^^^^
  Error: Unknown field "timing"
  [1]
  $ list_outputs
  find: '_build/default': No such file or directory
  [1]

The fields require dune 3.26:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.25)
  > (env
  >  (_
  >   (rocq
  >    (timing true))))
  > EOF

  $ dune build @all
  File "dune-workspace", line 5, characters 4-10:
  5 |    (timing true))))
          ^^^^^^
  Error: Unknown field "timing"
  [1]
