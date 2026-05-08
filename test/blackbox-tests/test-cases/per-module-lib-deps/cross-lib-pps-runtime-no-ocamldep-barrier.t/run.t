Regression guard for [build_lib_index]'s [no_ocamldep_lib]
classification: it must mirror [Dep_rules.skip_ocamldep]'s
[has_library_deps] gating, which uses the *resolved* requires
(including pps runtime libs added via [add_pp_runtime_deps]),
not the static [(libraries ...)] field. A single-module local
lib with no [(libraries ...)] but with [(preprocess (pps X))]
has non-empty resolved requires (X's runtime libs), so its
ocamldep is run and classifying it as [no_ocamldep] would cause
the cross-library walk to skip its post-pp ocamldep output —
dropping transitive [.cmi] deps from the consumer's compile rule
and breaking sandboxed builds.

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

[hello] is the ppx runtime lib (single-module unwrapped, no library
deps of its own — correctly classified as no-ocamldep). It exposes
[Hello.t]:

  $ mkdir hello
  $ cat > hello/dune <<EOF
  > (library (name hello) (wrapped false))
  > EOF
  $ cat > hello/hello.ml <<EOF
  > type t = int
  > let zero : t = 0
  > EOF

[hello_ppx] is a no-op ppx_rewriter declaring [hello] as its
[ppx_runtime_libraries]:

  $ mkdir hello_ppx
  $ cat > hello_ppx/dune <<EOF
  > (library
  >  (name hello_ppx)
  >  (kind ppx_rewriter)
  >  (ppx_runtime_libraries hello)
  >  (libraries ppxlib))
  > EOF
  $ cat > hello_ppx/hello_ppx.ml <<EOF
  > let () =
  >   Ppxlib.Driver.register_transformation_using_ocaml_current_ast
  >     ~impl:(fun s -> s) "noop"
  > EOF

[middle] is single-module, has no [(libraries ...)], and uses
[(preprocess (pps hello_ppx))]. Its interface mentions [Hello.t]:

  $ mkdir middle
  $ cat > middle/dune <<EOF
  > (library
  >  (name middle)
  >  (wrapped false)
  >  (preprocess (pps hello_ppx)))
  > EOF
  $ cat > middle/middle.mli <<EOF
  > val helper : Hello.t -> Hello.t
  > EOF
  $ cat > middle/middle.ml <<EOF
  > let helper x = x
  > EOF

[consumer] depends on [middle]; references [Middle.helper] but
never names [Hello] in source:

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (executable (name consumer) (libraries middle))
  > EOF
  $ cat > consumer/consumer.ml <<EOF
  > let _ = Middle.helper 0
  > EOF

Sandbox-forced build of the consumer succeeds: the walker visits
[middle]'s post-pp ocamldep, surfaces [Hello] in the tight set,
and the classification fold keeps [hello]'s [.cmi] as a compile-
rule dep so it is pulled into the sandbox.

  $ dune build --sandbox=copy consumer/consumer.exe
