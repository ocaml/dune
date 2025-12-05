By default the coqdep flags are empty.

  $ cp theories/dune.noflags theories/dune
  $ dune build theories/.basic.theory.d

We use non-exising coqdep flags, so compilation fails.

  $ mv dune.disabled dune
  $ dune build theories/.basic.theory.d
  Warning: Unknown option "--global-flag1". [unknown-option,default]
  Warning: Unknown option "--global-flag2". [unknown-option,default]

We then add more flags locally to the theory.

  $ rm -f theories/dune
  $ cp theories/dune.flags theories/dune
  $ dune build theories/.basic.theory.d
  Warning: Unknown option "--global-flag1". [unknown-option,default]
  Warning: Unknown option "--global-flag2". [unknown-option,default]
  Warning: Unknown option "--local-flag1". [unknown-option,default]
  Warning: Unknown option "--local-flag2". [unknown-option,default]

Finally we remove the toplevel dune file which sets some flags, but keep the
theory-local flags only.

  $ rm -f dune
  $ dune build theories/.basic.theory.d
  Warning: Unknown option "--local-flag1". [unknown-option,default]
  Warning: Unknown option "--local-flag2". [unknown-option,default]
