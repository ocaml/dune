Test `melange.runtime_deps` in a public library in the workspace

  $ mkdir prefix
  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (package (name foo))
  > (using melange 0.1)
  > EOF

  $ mkdir -p lib/nested
  $ echo "Some text" > lib/index.txt
  $ echo "Some nested text" > lib/nested/hello.txt
  $ cat > lib/dune <<EOF
  > (library
  >  (public_name foo)
  >  (modes melange)
  >  (libraries melange.node)
  >  (preprocess (pps melange.ppx))
  >  (melange.runtime_deps index.txt nested/hello.txt))
  > EOF
  $ cat > lib/foo.ml <<EOF
  > let dirname = [%mel.raw "__dirname"]
  > let () = Js.log2 "dirname:" dirname
  > let file_path = "./index.txt"
  > let read_asset () = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) \`utf8
  > EOF

  $ dune build
  $ dune install --prefix $PWD/prefix
  $ cat _build/default/foo.install
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
    "_build/install/default/lib/foo/foo.ml"
    "_build/install/default/lib/foo/index.txt"
    "_build/install/default/lib/foo/melange/foo.cmi" {"melange/foo.cmi"}
    "_build/install/default/lib/foo/melange/foo.cmj" {"melange/foo.cmj"}
    "_build/install/default/lib/foo/melange/foo.cmt" {"melange/foo.cmt"}
    "_build/install/default/lib/foo/nested/hello.txt" {"nested/hello.txt"}
  ]

  $ mkdir assets
  $ cat > assets/file.txt <<EOF
  > hello from file
  > EOF
  $ cat > dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (target output)
  >  (libraries foo melange.node)
  >  (preprocess (pps melange.ppx))
  >  (emit_stdlib false)
  >  (runtime_deps assets/file.txt))
  > EOF

  $ cat > main.ml <<EOF
  > let dirname = [%mel.raw "__dirname"]
  > let file_path = "./assets/file.txt"
  > let file_content = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) \`utf8
  > let () = Js.log file_content
  > let () = Js.log (Foo.read_asset ())
  > EOF

  $ dune build @mel

The runtime_dep index.txt was copied to the build folder

  $ ls _build/default/lib
  foo.ml
  foo.pp.ml
  index.txt
  nested
  $ ls _build/default/output/node_modules/foo/
  foo.js
  index.txt
  nested
  $ node _build/default/output/main.js
  dirname: $TESTCASE_ROOT/_build/default/output/node_modules/foo
  hello from file
  
  Some text
  
The same does not work for non-recursive aliases

  $ dune clean
  $ dune build @@mel
  $ ls _build/default/output
  main.js
  node_modules

Now try to depend on an external path in a public library
  $ cat > lib/dune <<EOF
  > (library
  >  (public_name foo)
  >  (modes melange)
  >  (melange.runtime_deps /etc/hosts))
  > EOF

Try to depend on it via `melange.emit`

  $ dune build @mel --display=short
  File "lib/dune", line 4, characters 23-33:
  4 |  (melange.runtime_deps /etc/hosts))
                             ^^^^^^^^^^
  Error: Public library foo depends on external path `/etc/hosts'. This is not
  allowed.
  Hint: Move the external dependency to the workspace and use a relative path.
  [1]

  $ rm -rf dune
  $ dune build @install
  File "lib/dune", line 4, characters 23-33:
  4 |  (melange.runtime_deps /etc/hosts))
                             ^^^^^^^^^^
  Error: Public library foo depends on external path `/etc/hosts'. This is not
  allowed.
  Hint: Move the external dependency to the workspace and use a relative path.
  [1]
