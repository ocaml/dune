
#  $ cat >dune <<EOF
#  > (rule
#  >  (action (with-stdout-to file (echo %{coq_version}))))
#  > EOF
#
#  $ dune build ./file
#  $ cat _build/default/file

  $ mkdir sub
  $ cat >sub/dune <<EOF
  > (rule
  >  (enabled_if (>= %{coq_version} 8.15))
  >  (action (with-stdout-to file (echo 123))))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (action (with-stdout-to alwayspresent (echo 123))))
  > EOF

  $ dune build ./alwayspresent

  (coq.theory
   (name foo)
   (native_theories mathcomp)
   (theories mathcomp))
