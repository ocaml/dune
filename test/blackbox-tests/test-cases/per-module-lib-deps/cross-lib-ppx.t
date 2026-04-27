Cross-library reads of ocamldep output must tolerate ppx-transformed
sources. When the target dependency library uses a preprocessor, its
[.d] file's left-hand basename is [foo.pp.ml] — the pp-transformed
source fed to ocamldep by the rule. The per-module filter's BFS in
the consumer holds a raw [Module.t] whose source path is [foo.ml],
so a strict basename check in the [.d] parser would mismatch and
raise "ocamldep returned unexpected output". The reader must parse
leniently; the producing rule already validates the basename.

See: https://github.com/ocaml/dune/pull/14116

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
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

[dep] is a tight-eligible library (local, unwrapped, multi-module so
ocamldep is not short-circuited) whose modules are preprocessed:

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

[consumer] references [Mod_a] from [dep]. Compiling [c.ml] triggers
the cross-library BFS to read [dep/mod_a.impl.d] — whose producer
ran ocamldep on [mod_a.pp.ml]. The BFS must read this [.d] with a
raw [Module.t] for [mod_a]:

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
