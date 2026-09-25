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
  $ cat _build/install/default/share/foo/output/top.txt
  top
  $ cat _build/install/default/share/foo/output/child/nested.txt
  nested

  $ mkdir installation
  $ dune install --prefix ./installation --display short
  Installing installation/lib/foo/META
  Installing installation/lib/foo/dune-package
  Installing installation/share/foo/output/child/nested.txt
  Installing installation/share/foo/output/top.txt
  $ cat installation/share/foo/output/top.txt
  top
  $ cat installation/share/foo/output/child/nested.txt
  nested
