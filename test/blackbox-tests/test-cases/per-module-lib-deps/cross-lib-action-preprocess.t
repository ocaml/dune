A consumer of a library that uses an action preprocessor builds
without error.

When the dep library uses [(preprocess (action ...))], dune feeds
the [.pp.ml]-form of dep's source to ocamldep, so dep's [.d] file's
left-hand basename is [foo.pp.ml] rather than [foo.ml]. Any future
inter-library dependency tracking that reads dep's [.d] files from
outside the producing rule must tolerate this mismatch with the raw
[Module.t]'s [.ml] source path; this test guards against breaking
consumers of action-preprocessed libs.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

A trivial preprocessor that passes its input through unchanged. The
critical property is that dune's pp pipeline writes a [.pp.ml] file
and feeds it to ocamldep, not the original [.ml].

  $ mkdir pp
  $ cat > pp/dune <<EOF
  > (executable (name pp))
  > EOF
  $ cat > pp/pp.ml <<EOF
  > let () =
  >   let ic = open_in_bin Sys.argv.(1) in
  >   try
  >     while true do print_char (input_char ic) done
  >   with End_of_file -> ()
  > EOF

[dep] is an unwrapped multi-module library whose modules are preprocessed:

  $ mkdir dep
  $ cat > dep/dune <<EOF
  > (library
  >  (name dep)
  >  (wrapped false)
  >  (preprocess (action (run %{exe:../pp/pp.exe} %{input-file}))))
  > EOF
  $ cat > dep/mod_a.ml <<EOF
  > let v = 1
  > EOF
  $ cat > dep/mod_b.ml <<EOF
  > let v = 2
  > EOF

[consumer] references [Mod_a] from [dep]:

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (library (name consumer) (wrapped false) (libraries dep))
  > EOF
  $ cat > consumer/c.ml <<EOF
  > let _ = Mod_a.v
  > EOF
  $ cat > consumer/d.ml <<EOF
  > let _ = ()
  > EOF

Build must succeed:

  $ dune build @check
