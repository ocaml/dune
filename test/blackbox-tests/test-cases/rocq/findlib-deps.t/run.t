Rocq theory dependencies are package names in Rocq language 0.15.

  $ make_rocq_project 3.26 0.15
  $ cat >> dune-project << EOF
  > (package (name pkg))
  > EOF
  $ touch pkg.opam

Workspace packages are resolved by Dune and passed as ordinary -Q flags.
Installed packages stay package-shaped and are passed to Rocq with -package.

  $ mkdir A B
  $ cat > A/dune << EOF
  > (rocq.theory
  >  (name A)
  >  (public_name pkg.a))
  > EOF
  $ cat > A/a.v << EOF
  > Definition x := 0.
  > EOF
  $ cat > B/dune << EOF
  > (rocq.theory
  >  (name B)
  >  (public_name pkg.b)
  >  (theories pkg.a rocq-stdlib))
  > EOF
  $ cat > B/b.v << EOF
  > From A Require Import a.
  > From Stdlib Require Import List.
  > Definition y := x.
  > Definition ys := map (fun x : nat => x) nil.
  > EOF

Two public theories may share a Rocq logical name when their public names do not
collide.

  $ mkdir A2
  $ cat > A2/dune << EOF
  > (rocq.theory
  >  (name A)
  >  (public_name pkg.a2))
  > EOF
  $ cat > A2/a2.v << EOF
  > Definition z := 2.
  > EOF
  $ dune build A2/a2.vo

  $ trace_rocq_flags B/b.vo | grep -o '"-package","rocq-stdlib"'
  "-package","rocq-stdlib"
  "-package","rocq-stdlib"
  $ trace_rocq_flags B/b.vo | grep '"-package","pkg.a"' || true

Packages with custom Rocq install entries are materialized before they are
passed to Rocq. This is needed by packages such as Rocq itself, whose theories
are built using custom rules rather than [rocq.theory].

  $ cat >> dune-project << EOF
  > (package
  >  (name prebuilt)
  >  (allow_empty))
  > EOF
  $ touch prebuilt.opam
  $ cat > META.prebuilt.template << EOF
  > rocqpath = "Prebuilt"
  > # DUNE_GEN
  > EOF
  $ mkdir Prebuilt Consumer
  $ cat > Prebuilt/dune << EOF
  > (library
  >  (name dummy)
  >  (public_name prebuilt))
  > (rocq.theory
  >  (name Prebuilt))
  > (install
  >  (section lib)
  >  (package prebuilt)
  >  (files
  >   (p.v as rocq.d/p.v)
  >   (p.vo as rocq.d/p.vo)))
  > EOF
  $ touch Prebuilt/dummy.ml
  $ cat > Prebuilt/p.v << EOF
  > Definition x := 42.
  > EOF
  $ cat > Consumer/dune << EOF
  > (rocq.theory
  >  (name Consumer)
  >  (theories prebuilt))
  > EOF
  $ cat > Consumer/use.v << EOF
  > From Prebuilt Require Import p.
  > Check x.
  > EOF
  $ dune build _build/install/default/lib/prebuilt/META
  $ OCAMLPATH=$PWD/_build/install/default/lib:$OCAMLPATH dune build Consumer/use.vo
  x
       : nat
