Dune should build coq projects even if ocamlc is absent

First, we hide everything except for coq, dune, and cat (because it's required for cram)

  $ mkdir _bin
  $ ln -s $(command -v coqc) _bin/coqc
  $ ln -s $(command -v coqdep) _bin/coqdep
  $ DUNE_DIR=$(dirname $(command -v dune))
  $ CAT_DIR=$(dirname $(command -v cat))
  $ export PATH=$PWD/_bin:$DUNE_DIR:$CAT_DIR

Now we create a dummy coq project

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > (using coq 0.3)
  > EOF

  $ cat >dune <<EOF
  > (coq.theory
  >  (name basic)
  >  (modules :standard)
  >  (synopsis "Test Coq library"))
  > EOF

And then make sure it builds

  $ dune build @all
