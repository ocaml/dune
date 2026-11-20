Package dependencies make the selected Rocq theories available to actions,
including theories reached through the dependency closure.

  $ cat >dune-project <<'EOF'
  > (lang dune 3.25)
  > (using rocq 0.12)
  > (package (name bottom))
  > (package (name top))
  > EOF

  $ mkdir bottom top inspect consumer
  $ cat >bottom/dune <<'EOF'
  > (rocq.theory
  >  (name Bottom)
  >  (package bottom))
  > EOF
  $ cat >bottom/Bottom.v <<'EOF'
  > Definition bottom := nat.
  > EOF

  $ cat >top/dune <<'EOF'
  > (rocq.theory
  >  (name Top)
  >  (package top)
  >  (theories Bottom))
  > EOF
  $ cat >top/Top.v <<'EOF'
  > From Bottom Require Import Bottom.
  > Definition top := bottom.
  > EOF

First inspect the environment and materialized package set directly.

  $ cat >inspect/dune <<'EOF'
  > (rule
  >  (alias inspect)
  >  (deps
  >   inspect.sh
  >   (package top))
  >  (action
  >   (run bash %{dep:inspect.sh})))
  > 
  > (rule
  >  (alias consume)
  >  (deps
  >   (source_tree ../consumer)
  >   (package top))
  >  (action
  >   (chdir ../consumer
  >    (run %{bin:dune} build))))
  > EOF
  $ cat >inspect/inspect.sh <<'EOF'
  > if [ -n "${ROCQPATH:-}" ]; then
  >   echo "ROCQPATH is set"
  > else
  >   echo "ROCQPATH is unset"
  > fi
  > root=${OCAMLPATH%%:*}
  > for theory in Top Bottom; do
  >   if [ -f "$root/coq/user-contrib/$theory/$theory.vo" ]; then
  >     echo "$theory is present"
  >   else
  >     echo "$theory is missing"
  >   fi
  > done
  > EOF

  $ dune build @inspect
  ROCQPATH is unset
  Top is present
  Bottom is missing

A nested Dune invocation must also be able to discover and use the theories.

  $ cat >consumer/dune-project <<'EOF'
  > (lang dune 3.25)
  > (using rocq 0.12)
  > EOF
  $ cat >consumer/dune <<'EOF'
  > (rocq.theory
  >  (name Consumer)
  >  (theories Top Bottom))
  > EOF
  $ cat >consumer/Consumer.v <<'EOF'
  > From Top Require Import Top.
  > Check top.
  > EOF

  $ dune build @consume
  File "dune", line 3, characters 11-14:
  3 |  (theories Top Bottom))
                 ^^^
  Theory "Top" has not been found.
  -> required by theory Consumer in dune:2
  -> required by _build/default/.Consumer.theory.d
  -> required by alias all
  -> required by alias default
  [1]
