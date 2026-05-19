Incremental-build regression guard for [(implements vlib)
(private_modules ...)]. Companion to [impl-private-modules.t], which
only checks the initial build's artifact layout. Here we exercise
the incremental case: editing the impl's private module's body must
not break the rebuild, and a downstream consumer that depends on the
[impl] library must continue to type-check.

  $ make_dune_project 3.24

[vlib] declares one virtual module [Bar]:

  $ mkdir vlib
  $ cat > vlib/dune <<EOF
  > (library
  >  (name vlib)
  >  (virtual_modules bar))
  > EOF
  $ cat > vlib/bar.mli <<EOF
  > val v : int
  > EOF

[impl] implements [vlib] and adds a private module [Priv]; [bar.ml]
references [Priv.helper]:

  $ mkdir impl
  $ cat > impl/dune <<EOF
  > (library
  >  (name impl)
  >  (implements vlib)
  >  (private_modules priv))
  > EOF
  $ cat > impl/bar.ml <<EOF
  > let v = Priv.helper
  > EOF
  $ cat > impl/priv.ml <<EOF
  > let helper = 1
  > EOF

[consumer] uses [Vlib]'s interface (which routes to [impl]'s [bar.ml]),
and a small executable observes the value [Vlib.Bar.v] resolves to
at runtime:

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (library
  >  (name consumer)
  >  (modules c)
  >  (libraries impl))
  > (executable
  >  (name main)
  >  (modules main)
  >  (libraries impl))
  > EOF
  $ cat > consumer/c.ml <<EOF
  > let _ = Vlib.Bar.v
  > EOF
  $ cat > consumer/main.ml <<EOF
  > let () = print_int Vlib.Bar.v; print_newline ()
  > EOF

Initial build succeeds (consumer type-checks; executable links and
observes the initial value of [Priv.helper]):

  $ dune build @check
  $ dune build ./consumer/main.exe
  $ ./_build/default/consumer/main.exe
  1

Mutate [priv.ml]'s body. The rebuild must succeed: the consumer
continues to type-check via [Vlib.Bar] (asserted by [@check]),
[main.exe] must re-link against the new [Priv.helper] (asserted by
its rebuild), and running the binary must now observe the updated
value (asserted by the runtime output below):

  $ cat > impl/priv.ml <<EOF
  > let helper = 2
  > EOF
  $ dune build @check
  $ dune build ./consumer/main.exe
  $ ./_build/default/consumer/main.exe
  2
