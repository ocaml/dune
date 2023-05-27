Testsuite for (mode plugin).

  $ cat > dune-project <<EOF
  > (lang dune 2.4)
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name a)
  >  (modules a)
  >  (modes plugin exe)
  >  (libraries foo))
  > 
  > (executable
  >  (name b)
  >  (modules b)
  >  (modes plugin)
  >  (libraries bar)
  >  (embed_in_plugin_libraries foo))
  > 
  > (rule
  >  (alias t)
  >  (action (echo "%{ext_plugin}\n")))
  > EOF

  $ cat > a.ml <<EOF
  > let () =
  >   Foo.f ()
  > EOF

  $ cat > b.ml <<EOF
  > let () =
  >   Foo.f ()
  > EOF

  $ mkdir foo main main2

  $ cat > foo/dune <<EOF
  > (library
  >  (name foo)
  >  (modules foo))
  > 
  > (library
  >  (name bar)
  >  (libraries foo)
  >  (modules bar))
  > EOF

  $ cat > foo/foo.ml <<EOF
  > let f () =
  >   print_endline "12"
  > EOF

  $ cat > foo/bar.ml <<EOF
  > let g () =
  >   Foo.f ()
  > EOF

  $ cat > main/dune <<EOF
  > (executable
  >  (name main)
  >  (link_flags -linkall)
  >  (libraries dynlink foo))
  > EOF

  $ cat > main/main.ml <<EOF
  > let () =
  >   Dynlink.loadfile (Dynlink.adapt_filename "a.cma")
  > EOF

  $ cat > main2/dune <<EOF
  > (executable
  >  (name main)
  >  (libraries dynlink))
  > EOF

  $ cat > main2/main.ml <<EOF
  > let () =
  >   Dynlink.loadfile (Dynlink.adapt_filename "b.cma")
  > EOF

  $ dune build --display short @all 2>&1 | dune_cmd sanitize | grep cmxs | sort
      ocamlopt a.cmxs
      ocamlopt b.cmxs
      ocamlopt foo/bar.cmxs
      ocamlopt foo/foo.cmxs

  $ (cd _build/default && main/main.exe)
  12

  $ (cd _build/default && main2/main.exe)
  12

  $ dune build @t
  .cmxs

  $ cat > dune <<EOF
  > (executable
  >  (name a)
  >  (modules a)
  >  (embed_in_plugin_libraries xxx))
  > EOF

  $ dune build @all
  File "dune", line 4, characters 28-31:
  4 |  (embed_in_plugin_libraries xxx))
                                  ^^^
  Error: This field can only be used when linking a plugin.
  [1]

  $ cat > dune <<EOF
  > (rule (with-stdout-to xxx.ml (echo "let () = print_endline \"Hello, xxx\"")))
  > (library
  >  (name xxx)
  >  (modules xxx))
  > (executable
  >  (name a)
  >  (modules a)
  >  (modes plugin)
  >  (libraries foo)
  >  (embed_in_plugin_libraries xxx))
  > EOF

  $ dune build @all

  $ cat > dune <<EOF
  > (rule (with-stdout-to xxx.ml (echo "let () = print_endline \"Hello, xxx\"")))
  > 
  > (library
  >  (name xxx)
  >  (modules xxx))
  > 
  > (executable
  >  (name a)
  >  (modules a)
  >  (link_flags -linkall)
  >  (modes plugin)
  >  (libraries foo)
  >  (embed_in_plugin_libraries xxx))
  > EOF

  $ dune build @all
  $ (cd _build/default && main/main.exe)
  Hello, xxx
  12
