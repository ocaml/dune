Test the needed_deps action, which declares the dependencies written
in the passed files as needed dependencies for the rule. The tests are
made for all the constructors.

  $ generatefile () {
  > cat >$1 <<EOF
  > contents of $2
  > EOF
  > }

  $ cat >dune-project <<EOF
  > (lang dune 3.18)
  > EOF

  $ generatefile a a_now

  $ generatefile b b_now

  $ generatefile c c_now

  $ generatefile d d_now

  $ cat >dune <<EOF
  > (alias
  >  (name test) (deps c))
  > (rule
  >  (alias foo)
  >  (deps (order_only (file a) (file b) (file d) (alias test)))
  >  (action
  >  (progn
  >  (echo "executing foo")
  >  (with-stdout-to deps (echo "(file b) (alias test) (file_selector d)"))
  >  (needed_deps deps))))
  > EOF

  $ dune build @foo
  executing foo

  $ generatefile a a_changed

  $ dune build @foo

  $ generatefile b b_changed

  $ dune build @foo
  executing foo

  $ generatefile c c_changed

  $ dune build @foo
  executing foo

  $ generatefile d d_changed

  $ dune build @foo
  executing foo
