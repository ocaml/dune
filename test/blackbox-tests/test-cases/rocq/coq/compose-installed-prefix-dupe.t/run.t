If we have installed theories A.B and A.C then Dune should not complain that A
is a duplicate theory.

First we install our two theories with the conflicting name prefix.

  $ (cd B && dune build @install && dune install --prefix .)
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  $ (cd C && dune build @install && dune install --prefix .)
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))

We add these to COQPATH

  $ export COQPATH=../B/lib/coq/user-contrib:../C/lib/coq/user-contrib:${COQPATH:-}

Now we create a theory that depends on both

  $ mkdir mytheory && cd mytheory

  $ cat > dune-project << EOF
  > (lang dune 3.8)
  > (using coq 0.8)
  > EOF

  $ cat > dune << EOF
  > (coq.theory
  >  (name mytheory)
  >  (theories A.B A.C))
  > EOF

  $ cat > a.v << EOF
  > From A.B Require Import a.
  > From A.C Require Import a.
  > Print b.
  > EOF

  $ dune build a.vo
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  Inductive b : Prop :=  .
 
