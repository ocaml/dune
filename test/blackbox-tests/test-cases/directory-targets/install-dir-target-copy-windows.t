On Windows, dune does not create directory symlinks. This verifies that the
output directory is recursively copied

  $ cat > dune-project <<EOF
  > (lang dune 3.5)
  > (package (name foo))
  > (using directory-targets 0.1)
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name gen))
  > (rule
  >  (targets (dir output))
  >  (action (run ./gen.exe)))
  > (install
  >  (section share)
  >  (dirs output))
  > EOF

  $ cat > gen.ml <<EOF
  > let write path contents =
  >   let oc = open_out path in
  >   output_string oc contents;
  >   close_out oc
  > let () =
  >   Sys.mkdir "output" 0o755;
  >   Sys.mkdir (Filename.concat "output" "child") 0o755;
  >   write (Filename.concat "output" "top.txt") "top\n";
  >   write (Filename.concat "output" (Filename.concat "child" "nested.txt")) "nested\n"
  > EOF

  $ dune build @install
  Error: _build/default/output: Permission denied
  -> required by _build/install/default/share/foo/output
  -> required by _build/default/foo.install
  -> required by alias install
  [1]
  $ cat _build/install/default/share/foo/output/top.txt
  cat: _build/install/default/share/foo/output/top.txt: No such file or directory
  [1]
  $ cat _build/install/default/share/foo/output/child/nested.txt
  cat: _build/install/default/share/foo/output/child/nested.txt: No such file or directory
  [1]
  $ mkdir installation
  $ dune install --prefix ./installation --display short
  Error: The following <package>.install are missing:
  - _build/default/foo.install
  Hint: try running 'dune build [-p <pkg>] @install'
  [1]
  $ cat installation/share/foo/output/top.txt
  cat: installation/share/foo/output/top.txt: No such file or directory
  [1]
  $ cat installation/share/foo/output/child/nested.txt
  cat: installation/share/foo/output/child/nested.txt: No such file or directory
  [1]
