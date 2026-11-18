Per-file timing (rocq compile -time-file) and profiling (rocq compile
-profile) outputs, enabled through the env stanza or rocq.theory stanza.

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

Enabling them in the workspace only for the dev profile:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.26)
  > (env
  >  (_
  >   (rocq
  >    (timing (= %{profile} dev))
  >    (profiling (= %{profile} dev)))))
  > EOF

  $ dune build --profile release @all
  $ list_outputs

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

Managed timing and profiling outputs take precedence over raw Rocq flags:

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name basic)
  >  (flags (:standard -time))
  >  (modules_flags (foo (:standard -profile custom.prof.json))))
  > EOF

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

The rocq.theory fields take precedence over the env stanza:

  $ cat > dune <<EOF
  > (env
  >  (_
  >   (rocq
  >    (profiling false))))
  > (rocq.theory
  >  (name basic)
  >  (timing (= %{profile} release))
  >  (profiling (= %{profile} dev)))
  > EOF

  $ dune build @all
  $ list_outputs
  _build/default/bar.vo.prof.json
  _build/default/foo.vo.prof.json

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

This also applies to the fields in the rocq.theory stanza:

  $ rm dune-workspace
  $ cat > dune-project <<EOF
  > (lang dune 3.25)
  > (using rocq 0.11)
  > EOF
  $ cat > dune <<EOF
  > (rocq.theory
  >  (name basic)
  >  (timing true))
  > EOF

  $ dune build @all
  File "dune", line 3, characters 1-14:
  3 |  (timing true))
       ^^^^^^^^^^^^^
  Error: 'timing' is only available since version 3.26 of the dune language.
  Please update your dune-project file to have (lang dune 3.26).
  [1]
