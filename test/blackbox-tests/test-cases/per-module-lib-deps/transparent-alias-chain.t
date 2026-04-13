Transparent module aliases create cross-library .cmi reads that ocamldep
does not report. This test documents the behavior: ocamldep only sees the
direct library reference, but the compiler follows alias chains and reads
.cmi files from transitive libraries at compile time.

Any per-module inter-library dependency optimization must account for this.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

Set up a chain: libA -> libB -> libC -> libD, where each intermediate
library creates a transparent alias to the next.

  $ mkdir libd
  $ cat > libd/dune <<EOF
  > (library (name libd))
  > EOF
  $ cat > libd/leaf.ml <<EOF
  > let v = 99
  > EOF

  $ mkdir libc
  $ cat > libc/dune <<EOF
  > (library (name libc) (libraries libd))
  > EOF
  $ cat > libc/mid.ml <<EOF
  > module L = Libd.Leaf
  > EOF

  $ mkdir libb
  $ cat > libb/dune <<EOF
  > (library (name libb) (libraries libc))
  > EOF
  $ cat > libb/bridge.ml <<EOF
  > module M = Libc.Mid
  > EOF

  $ mkdir liba
  $ cat > liba/dune <<EOF
  > (library (name liba) (libraries libb))
  > EOF
  $ cat > liba/consumer.ml <<EOF
  > let y = Libb.Bridge.M.L.v
  > EOF

ocamldep only reports the direct reference (Libb), not the transitive
libraries reached through aliases:

  $ ocamldep -modules liba/consumer.ml
  liba/consumer.ml: Libb

But the build succeeds because dune ensures all transitive library .cmi
files are available:

  $ dune build liba/.liba.objs/byte/liba__Consumer.cmo

Verify that the compilation rule depends on .cmi files from all libraries
in the chain, not just the directly referenced one:

  $ dune rules liba/.liba.objs/byte/liba__Consumer.cmo 2>&1 | grep -o 'lib[a-d]\.' | sort -u
  liba.
  libb.
  libc.
  libd.
