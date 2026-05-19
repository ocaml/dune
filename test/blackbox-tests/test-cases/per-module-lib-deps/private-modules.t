A consumer of an unwrapped library that has [private_modules] builds
without error.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

[dep]: unwrapped library with one public entry module [Pub] and one
private module [Priv]. Consumers can see [Pub] via [-I] search but
not [Priv]:

  $ mkdir dep
  $ cat > dep/dune <<EOF
  > (library
  >  (name dep)
  >  (wrapped false)
  >  (private_modules priv))
  > EOF
  $ cat > dep/pub.ml <<EOF
  > let greeting () = Priv.helper ^ "!"
  > EOF
  $ cat > dep/priv.ml <<EOF
  > let helper = "hi"
  > EOF

[consumer]: references [Pub] from [dep]:

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (library (name consumer) (wrapped false) (libraries dep))
  > EOF
  $ cat > consumer/c.ml <<EOF
  > let v = Pub.greeting ()
  > EOF
  $ cat > consumer/d.ml <<EOF
  > let _ = ()
  > EOF

Build must succeed. Sandbox is forced on so the test fails reliably
if the consumer's compile rule doesn't pull the public-cmi rule into
the sandbox; without forced sandboxing the public cmi might be found
on disk from a sibling rule and silently mask the regression.

  $ DUNE_SANDBOX=symlink dune build @check
