Test that we can control generation of cmt files using (cmt_annot ...) field in (env ...).

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package (name pub))
  > (package (name pub2))
  > EOF

  $ mkdir -p priv/priv2 pub/pub2 priv/proj

  $ touch main.ml priv/priv.ml priv/priv2/priv2.ml pub/pub.ml pub/pub2/pub2.ml priv/proj/proj.ml

  $ cat >dune <<EOF
  > (library (name main))
  > EOF

  $ cat >priv/dune <<EOF
  > (env (_ (bin_annot false)))
  > (library (name priv))
  > EOF

  $ cat >priv/priv2/dune <<EOF
  > (env (_ (bin_annot true)))
  > (library (name priv2))
  > EOF

We also check that public libraries work: the first should be installed without
any cmt file...

  $ cat >pub/dune <<EOF
  > (env (_ (bin_annot false)))
  > (library (public_name pub))
  > EOF

... and the second one _with_ cmt files.

  $ cat >pub/pub2/dune <<EOF
  > (env (_ (bin_annot true)))
  > (library (public_name pub2))
  > EOF

  $ cat >priv/proj/dune-project <<EOF
  > (lang dune 3.8)
  > EOF

  $ cat >priv/proj/dune <<EOF
  > (library (name proj))
  > EOF

  $ dune build

Note that "pub" does not appear in the list (as we disabled cmt files for it).

  $ find _build -name '*.cmt*' | sort
  _build/default/.main.objs/byte/main.cmt
  _build/default/priv/priv2/.priv2.objs/byte/priv2.cmt
  _build/default/priv/proj/.proj.objs/byte/proj.cmt
  _build/default/pub/pub2/.pub2.objs/byte/pub2.cmt
  _build/install/default/lib/pub2/pub2.cmt

The next test shows that workspace env settings are used as fallback if a
project does not specify the option explicitly.

  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > (env (_ (bin_annot false)))
  > EOF

  $ dune build

  $ find _build -name '*.cmt*' | sort
  _build/default/priv/priv2/.priv2.objs/byte/priv2.cmt
  _build/default/pub/pub2/.pub2.objs/byte/pub2.cmt
  _build/install/default/lib/pub2/pub2.cmt
