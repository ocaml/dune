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

  $ dune build --display short @all
      ocamldep foo/.foo.objs/foo.ml.d
      ocamldep $ext_lib.eobjs/a.ml.d
      ocamldep foo/.bar.objs/bar.ml.d
      ocamldep .b.eobjs/b.ml.d
      ocamldep main/.main.eobjs/main.ml.d
      ocamldep main2/.main.eobjs/main.ml.d
        ocamlc foo/.foo.objs/byte/foo.{cmi,cmo,cmt}
        ocamlc main2/.main.eobjs/byte/dune__exe__Main.{cmi,cmo,cmt}
        ocamlc foo/foo.cma
      ocamlopt foo/.foo.objs/native/foo.{cmx,o}
        ocamlc foo/.bar.objs/byte/bar.{cmi,cmo,cmt}
        ocamlc main/.main.eobjs/byte/dune__exe__Main.{cmi,cmo,cmt}
        ocamlc $ext_lib.eobjs/byte/dune__exe__A.{cmi,cmo,cmt}
      ocamlopt main2/.main.eobjs/native/dune__exe__Main.{cmx,o}
      ocamlopt foo/foo.{a,cmxa}
        ocamlc foo/bar.cma
      ocamlopt foo/.bar.objs/native/bar.{cmx,o}
        ocamlc .b.eobjs/byte/dune__exe__B.{cmi,cmo,cmt}
      ocamlopt main/.main.eobjs/native/dune__exe__Main.{cmx,o}
      ocamlopt $ext_lib.eobjs/native/dune__exe__A.{cmx,o}
      ocamlopt main2/main.exe
      ocamlopt foo/foo.cmxs
      ocamlopt foo/bar.{a,cmxa}
      ocamlopt .b.eobjs/native/dune__exe__B.{cmx,o}
      ocamlopt main/main.exe
      ocamlopt a.exe
      ocamlopt a.cmxs
      ocamlopt foo/bar.cmxs
      ocamlopt b.cmxs

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
