Two opaque locked packages each install a library that requires a library
shipped with the compiler. A consumer of both must get that library once, from
the compiler, not once from each package.

  $ make_lockdir
  $ for p in left right; do
  > make_lockpkg $p <<EOF
  > (version 1.0)
  > (build
  >  (progn
  >   (write-file META "requires = \"str\"\n")
  >   (write-file $p.install "lib: [ \"META\" ]")))
  > EOF
  > done
  $ make_named_package_project main 3.25 left right
  $ echo '(executable (name main) (libraries left right))' > dune
  $ echo 'let () = print_endline (Str.global_replace (Str.regexp "a") "b" "aaa")' > main.ml
  $ dune build main.exe
  $ _build/default/main.exe
  bbb
