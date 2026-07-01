Rocq theory dependencies are package names in Rocq language 0.15.

  $ make_rocq_project 3.25 0.15
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
