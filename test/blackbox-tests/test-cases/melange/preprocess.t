Test (preprocess) field on melange.emit stanza

  $ make_melange_project 3.8 0.1

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (modules main)
  >  (alias mel)
  >  (emit_stdlib false)
  >  (preprocess
  >   (action
  >    (run cat %{input-file}))))
  > EOF

  $ cat > main.ml <<EOF
  > let () =
  >   print_endline "hello"
  > EOF

  $ dune build @mel
  $ node _build/default/output/main.js
  hello

Reason sources with PPX preprocessing should pass the OCaml AST produced by the
Reason dialect preprocessor to the PPX driver.

  $ mkdir -p ppx reason-pps

  $ cat > ppx/dune <<'EOF'
  > (library
  >  (name reason_ppx)
  >  (kind ppx_rewriter)
  >  (libraries ppxlib))
  > EOF

  $ cat > ppx/reason_ppx.ml <<'EOF'
  > let () =
  >   let rec loop = function
  >     | [] -> ()
  >     | "-loc-filename" :: _ :: rest -> loop rest
  >     | arg :: rest ->
  >       if Filename.check_suffix arg ".re" || Filename.check_suffix arg ".rei"
  >       then (
  >         Printf.eprintf "ppx saw Reason source: %s\n" arg;
  >         exit 1)
  >       else loop rest
  >   in
  >   loop (Array.to_list Sys.argv)
  > ;;
  > let () = Ppxlib.Driver.register_transformation "reason_ppx"
  > EOF

  $ cat > reason-pps/dune <<'EOF'
  > (melange.emit
  >  (target out)
  >  (modules main)
  >  (alias mel)
  >  (emit_stdlib false)
  >  (preprocess (pps reason_ppx)))
  > EOF

  $ cat > reason-pps/main.re <<'EOF'
  > let add x y = x + y
  > let result = add 1 2
  > EOF

  $ dune build @reason-pps/mel

Qualified module references work in a `melange.emit` stanza too:

  $ mkdir qualified
  $ cd qualified
  $ make_melange_project 3.25 1.0
  $ mkdir foo
  $ echo 'This is not OCaml.' >foo/bar.ml
  $ cat >dune <<'EOF'
  > (include_subdirs qualified)
  > (melange.emit
  >  (target output)
  >  (modules Foo.Bar)
  >  (alias mel)
  >  (emit_stdlib false)
  >  (preprocess
  >   (per_module
  >    ((action
  >      (run echo "let marker = \"qualified\"")) Foo.Bar))))
  > EOF
  $ dune build @mel
  $ cd ..

Mixed libraries can select disjoint module sets for OCaml and Melange. Shared
lint settings may refer to modules from either set, including `.melange.ml`
sources:

  $ mkdir mixed
  $ cat >mixed/dune-project <<'EOF'
  > (lang dune 3.25)
  > (using melange 1.0)
  > EOF
  $ touch mixed/a.ml mixed/b.melange.ml mixed/excluded.ml
  $ cat >mixed/dune <<'EOF'
  > (library
  >  (name x)
  >  (modes byte melange)
  >  (modules A)
  >  (melange.modules B)
  >  (lint (per_module ((action (run true)) A B))))
  > EOF
  $ dune build --root=mixed @all

The same holds for preprocessing inherited by Melange:

  $ cat >mixed/dune <<'EOF'
  > (library
  >  (name x)
  >  (modes byte melange)
  >  (modules A)
  >  (melange.modules B)
  >  (preprocess (per_module ((action (run cat %{input-file})) A B))))
  > EOF
  $ dune build --root=mixed @all

Explicit mode-specific preprocessors may also mention modules owned by the
other mode; those mappings are simply unused in this mode:

  $ cat >mixed/dune <<'EOF'
  > (library
  >  (name x)
  >  (modes byte melange)
  >  (modules A)
  >  (melange.modules B)
  >  (preprocess (per_module ((action (run cat %{input-file})) A B)))
  >  (melange.preprocess (per_module ((action (run cat %{input-file})) A B))))
  > EOF
  $ dune build --root=mixed @all

A reference missing from both modes is currently accepted silently:

  $ cat >mixed/dune <<'EOF'
  > (library
  >  (name x)
  >  (modes byte melange)
  >  (modules A)
  >  (melange.modules B)
  >  (preprocess (per_module ((action (run cat %{input-file})) Missing))))
  > EOF
  $ dune build --root=mixed @all

A module excluded from both modes is also silently accepted:

  $ cat >mixed/dune <<'EOF'
  > (library
  >  (name x)
  >  (modes byte melange)
  >  (modules A)
  >  (melange.modules B)
  >  (lint (per_module ((action (run true)) Excluded))))
  > EOF
  $ dune build --root=mixed @all

Modules selected only by a disabled mode should not count as library members,
but their references are currently accepted:

  $ cat >mixed/dune <<'EOF'
  > (library
  >  (name x)
  >  (modes byte)
  >  (modules A)
  >  (melange.modules B)
  >  (preprocess (per_module ((action (run cat %{input-file})) B))))
  > EOF
  $ dune build --root=mixed @all

  $ cat >mixed/dune <<'EOF'
  > (library
  >  (name x)
  >  (modes melange)
  >  (modules A)
  >  (melange.modules B)
  >  (preprocess (per_module ((action (run cat %{input-file})) A))))
  > EOF
  $ dune build --root=mixed @all
