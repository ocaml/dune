Dune should attach synopsis to target that produces directory.

  $ cat > dune-project << EOF
  > (lang dune 3.18)
  > (using directory-targets 0.1)
  > EOF
  $ cat > dune << EOF
  > (rule
  >  (targets (dir created_dir/))
  >  (alias synopsis-for-dir)
  >  (synopsis "Synopsis for rule with directory target")
  >  (action 
  >   (progn 
  >     (run mkdir created_dir)
  >     (run touch created_dir/empty-file))))
  > EOF

  $ dune show targets
  created_dir/
    - dune:1 Synopsis for rule with directory target
  dune
  dune-project
