  $ mkdir 2.3 2.4
  $ cat > 2.3/dune-project <<EOF
  > (lang dune 2.3)
  > EOF
  $ cat > 2.3/dune <<EOF
  > (rule
  >   (deps source)
  >   (targets target)
  >   (action (bash "cat source source > target")))
  > EOF
  $ cat > 2.3/source <<EOF
  > \_o< COIN
  > EOF
  $ dune build --root 2.3 target
  Entering directory '2.3'
  $ ./stat.sh --format=%a 2.3/_build/default/target | head -c 1
  6
  $ cat > 2.4/dune-project <<EOF
  > (lang dune 2.4)
  > EOF
  $ cat > 2.4/dune <<EOF
  > (rule
  >   (deps source)
  >   (targets target)
  >   (action (bash "cat source source > target")))
  > EOF
  $ cat > 2.4/source <<EOF
  > \_o< COIN
  > EOF
  $ dune build --root 2.4 target
  Entering directory '2.4'
  $ ./stat.sh --format=%a 2.4/_build/default/target | head -c 1
  4
