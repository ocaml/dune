Issue #3336 describes a bug where it's not possible to use dune_build_info from
ppx binaries.

Here we demonstrate that such a ppx .exe is built successfully.

  $ dune exec ./executable/exec.exe >/dev/null 2>&1 --verbose
  [1]

  $ find _build | grep \.exe$
  _build/default/.ppx/98cd9c27bc47def1a842c7a721af4e6b/ppx.exe
