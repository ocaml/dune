Test simple interactions between melange.emit and copy_files

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > (using directory-targets 0.1)
  > EOF

  $ cat > dune <<EOF
  > (rule (target (dir some_dir))
  >  (action
  >   (progn (system "mkdir %{target}")
  >    (system "echo hello from file inside dir target > %{target}/inside-dir-target.txt"))))
  > (melange.emit
  >  (alias mel)
  >  (target output)
  >  (emit_stdlib false)
  >  (preprocess (pps melange.ppx))
  >  (runtime_deps ./some_dir))
  > EOF

  $ cat > main.ml <<EOF
  > external readFileSync : string -> encoding:string -> string = "readFileSync"
  > [@@mel.module "fs"]
  > let dirname = [%mel.raw "__dirname"]
  > let file_path = "./some_dir/inside-dir-target.txt"
  > let file_content = readFileSync (dirname ^ "/" ^ file_path) ~encoding:"utf8"
  > let () = Js.log file_content
  > EOF

  $ dune build @mel

Depend on a path inside the directory target

  $ cat > dune <<EOF
  > (rule (target (dir some_dir))
  >  (action
  >   (progn
  >    (system "mkdir -p %{target}/other/nested")
  >    (system "echo hello from file inside dir target > %{target}/other/nested/inside-dir-target.txt"))))
  > (melange.emit
  >  (alias mel)
  >  (target output)
  >  (emit_stdlib false)
  >  (preprocess (pps melange.ppx))
  >  (runtime_deps ./some_dir/other/nested/inside-dir-target.txt))
  > EOF
  $ dune build @mel

Dune symlinks the entire directory target

  $ ls _build/default/output
  main.js
  some_dir

  $ realpath _build/default/output/some_dir
  $TESTCASE_ROOT/_build/default/some_dir
