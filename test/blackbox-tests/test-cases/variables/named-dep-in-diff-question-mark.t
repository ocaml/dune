Regression test for using %{test} in (diff ...)

The action expander treats the second argument of diff? as "consuming
a target". Since targets needs to be known at rule creation time
rather than at rule evaluation time and dependencies are usually
evaluated at the latter stage, the below pattern could break if we
are not careful. We want to support it because it is a common pattern.

  $ echo '(lang dune 2.8)' > dune-project
  $ cat > dune <<EOF
  > (rule
  >  (alias runtest)
  >  (deps
  >   (:x test.ml))
  >  (action
  >   (diff? %{x} %{x}.corrected)))
  > EOF
  $ touch test.ml

  $ dune runtest
