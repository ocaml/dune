Tests for setting arbitrary solver variables.

  $ . ./helpers.sh
  $ mkrepo
 
  $ mkpkg a <<EOF
  > depends: [
  >  "b" {custom_flag}
  >  "c" {custom_string = "foo"}
  >  "d" {custom_string = "bar"}
  > ]
  > EOF
  $ mkpkg b <<EOF
  > EOF
  $ mkpkg c <<EOF
  > EOF
  $ mkpkg d <<EOF
  > EOF

Initial solve with no user variables set.
  $ solve a
  Solution for dune.lock:
  - a.0.0.1

Set some user variables and solve again.
  $ cat >dune-workspace <<EOF
  > (lang dune 3.11)
  > (context
  >  (default
  >   (solver_user_vars
  >    (custom_flag true)
  >    (custom_string foo))))
  > EOF

  $ solve a
  Solution for dune.lock:
  - a.0.0.1
  - b.0.0.1
  - c.0.0.1

Set some user variables to different values and solve again.
  $ cat >dune-workspace <<EOF
  > (lang dune 3.11)
  > (context
  >  (default
  >   (solver_user_vars
  >    (custom_flag false)
  >    (custom_string bar))))
  > EOF

  $ solve a
  Solution for dune.lock:
  - a.0.0.1
  - d.0.0.1
