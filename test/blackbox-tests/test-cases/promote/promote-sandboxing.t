Demonstrate a promotion going missing because of sandboxing

  $ function runtest() {
  > make_dune_project 3.22
  > touch bar
  > cat >dune <<EOF
  > (rule
  >  (target foo)
  >  (deps bar $1)
  >  (action
  >   (progn
  >    (with-stdout-to foo (echo "foo"))
  >    (diff bar foo))))
  > EOF
  > echo building ...
  > dune build foo
  > dune trace cat | jq 'select(.cat == "promote") | .args'
  > echo promotions:
  > dune promotion list
  > echo promoting ...
  > dune promote
  > rm -rf *
  > }

  $ runtest ""
  building ...
  File "bar", line 1, characters 0-0:
  --- bar
  +++ foo
  @@ -0,0 +1 @@
  +foo
  \ No newline at end of file
  {
    "src": "_build/default/foo",
    "dst": "bar",
    "how": "staged"
  }
  promotions:
  bar
  promoting ...
  Promoting _build/default/foo to bar.

  $ runtest "(sandbox always)"
  building ...
  File "bar", line 1, characters 0-0:
  --- bar
  +++ foo
  @@ -0,0 +1 @@
  +foo
  \ No newline at end of file
  {
    "src": "_build/default/foo",
    "dst": "bar",
    "how": "staged"
  }
  promotions:
  bar
  promoting ...
  Promoting _build/default/foo to bar.
