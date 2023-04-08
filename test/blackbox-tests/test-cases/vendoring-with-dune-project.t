Show that dune is unable to find private libraries in sub-directories with
dune-project files

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ mkdir -p src vendor/mylib/src

  $ cat > vendor/mylib/src/dune <<EOF
  > (library
  >  (name mylib))
  > EOF
  $ cat > vendor/mylib/src/mylib.ml <<EOF
  > let hello = "hello"
  > EOF

  $ cat > src/foo.ml <<EOF
  > let () = print_endline Mylib.hello
  > EOF
  $ cat > src/dune <<EOF
  > (executable
  >  (name foo)
  >  (libraries mylib))
  > EOF

  $ dune build

If we add a `dune-project` file to mylib it ceases to work

  $ cat > vendor/mylib/dune-project <<EOF
  > (lang dune 3.8)
  > EOF

  $ dune build
  File "src/dune", line 3, characters 12-17:
  3 |  (libraries mylib))
                  ^^^^^
  Error: Library "mylib" not found.
  -> required by _build/default/src/.foo.eobjs/byte/dune__exe__Foo.cmi
  -> required by _build/default/src/.foo.eobjs/native/dune__exe__Foo.cmx
  -> required by _build/default/src/foo.exe
  -> required by alias src/all
  -> required by alias default
  [1]

Adding mylib to vendored_dirs makes doesn't make it work either

  $ cat >vendor/dune <<EOF
  > (vendored_dirs *)
  > EOF

  $ dune build
  File "src/dune", line 3, characters 12-17:
  3 |  (libraries mylib))
                  ^^^^^
  Error: Library "mylib" not found.
  -> required by _build/default/src/.foo.eobjs/byte/dune__exe__Foo.cmi
  -> required by _build/default/src/.foo.eobjs/native/dune__exe__Foo.cmx
  -> required by _build/default/src/foo.exe
  -> required by alias src/all
  -> required by alias default
  [1]

Removing the dune-project file does


  $ rm -rf ./vendor/mylib/dune-project
  $ dune build

