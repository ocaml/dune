  $ make_instrumentation_backends
  $ make_dependency_instrumentation_project
  $ default_exe=./main.exe
  $ built_default_exe=_build/default/main.exe
  $ coverage_exe=_build/coverage/main.exe

Instrumentation can also be controlled by using the dune-workspace file.

  $ cat >dune-workspace <<EOF
  > (lang dune 2.7)
  > (instrument_with hello)
  > EOF

  $ dune build "$default_exe"

  $ "$built_default_exe"
  Hello from Spain (really)!

It can also be controlled on a per-context scope.

  $ cat >dune-workspace <<EOF
  > (lang dune 2.7)
  > (context (default (name coverage) (instrument_with hello)))
  > EOF

  $ dune build "$coverage_exe"

  $ "$coverage_exe"
  Hello from Spain (really)!

Per-context setting takes precedence over per-workspace setting.

  $ cat >dune-workspace <<EOF
  > (lang dune 2.7)
  > (instrument_with hello)
  > (context (default (name coverage) (instrument_with)))
  > EOF

  $ dune build "$coverage_exe"

  $ "$coverage_exe"
