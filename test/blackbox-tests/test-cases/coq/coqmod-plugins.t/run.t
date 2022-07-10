We test the detection of plugins by coqmod

  $ cat > dune << EOF
  > (coq.theory
  >  (name my_theory)
  >  (package my-package)
  >  (plugins my-package.my-plugin))
  > EOF

  $ cat > a.v << EOF
  > Declare ML Module "my-package.my-plugin".
  > EOF

  $ dune build
  File "./a.v", line 1, characters 0-41:
  Error:
  File not found on loadpath : my-package.my-plugin.cmxs
  Loadpath: $TESTCASE_ROOT/_build/default:$TESTCASE_ROOT/_build/default/src:/mnt/sda1/.opam/coq.8.15.1/lib/coq/user-contrib/Ltac2:/mnt/sda1/.opam/coq.8.15.1/lib/coq/user-contrib/mathcomp/ssreflect:/mnt/sda1/.opam/coq.8.15.1/lib/coq/user-contrib/mathcomp:/mnt/sda1/.opam/coq.8.15.1/lib/coq-core/plugins/zify:/mnt/sda1/.opam/coq.8.15.1/lib/coq-core/plugins/ssrmatching:/mnt/sda1/.opam/coq.8.15.1/lib/coq-core/plugins/ring:/mnt/sda1/.opam/coq.8.15.1/lib/coq-core/plugins/number_string_notation:/mnt/sda1/.opam/coq.8.15.1/lib/coq-core/plugins/btauto:/mnt/sda1/.opam/coq.8.15.1/lib/coq-core/plugins/rtauto:/mnt/sda1/.opam/coq.8.15.1/lib/coq-core/plugins/nsatz:/mnt/sda1/.opam/coq.8.15.1/lib/coq-core/plugins/tutorial/p2:/mnt/sda1/.opam/coq.8.15.1/lib/coq-core/plugins/tutorial/p3:/mnt/sda1/.opam/coq.8.15.1/lib/coq-core/plugins/tutorial/p0:/mnt/sda1/.opam/coq.8.15.1/lib/coq-core/plugins/tutorial/p1:/mnt/sda1/.opam/coq.8.15.1/lib/coq-core/plugins/tutorial:/mnt/sda1/.opam/coq.8.15.1/lib/coq-core/plugins/extraction:/mnt/sda1/.opam/coq.8.15.1/lib/coq-core/plugins/ssreflect:/mnt/sda1/.opam/coq.8.15.1/lib/coq-core/plugins/derive:/mnt/sda1/.opam/coq.8.15.1/lib/coq-core/plugins/tauto:/mnt/sda1/.opam/coq.8.15.1/lib/coq-core/plugins/ltac2:/mnt/sda1/.opam/coq.8.15.1/lib/coq-core/plugins/micromega:/mnt/sda1/.opam/coq.8.15.1/lib/coq-core/plugins/firstorder:/mnt/sda1/.opam/coq.8.15.1/lib/coq-core/plugins/ltac:/mnt/sda1/.opam/coq.8.15.1/lib/coq-core/plugins/cc:/mnt/sda1/.opam/coq.8.15.1/lib/coq-core/plugins/funind.
  
  [1]

Omitting a plugin from the plugins field is an error

  $ cat > dune << EOF
  > (coq.theory
  >  (name my_theory)
  >  (package my-package))
  > EOF

  $ dune build
  File "_build/default/a.v", line 1, characters 18-40:
  1 | Declare ML Module "my-package.my-plugin".
                        ^^^^^^^^^^^^^^^^^^^^^^
  Error: TODO undelcared plugin
  [1]

