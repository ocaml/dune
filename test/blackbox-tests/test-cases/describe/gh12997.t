`dune describe workspace` should not fail on missing external libraries.

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > EOF

  $ cat > dune << EOF
  > (library
  >  (name alive)
  >  (modules alive))
  > 
  > (library
  >  (name foo)
  >  (modules foo)
  >  (libraries does_not_exist_gh12997))
  > EOF

  $ cat > alive.ml << EOF
  > let x = 1
  > EOF

  $ cat > foo.ml << EOF
  > let x = 1
  > EOF

  $ dune describe workspace --sanitize-for-tests
  ((root /WORKSPACE_ROOT)
   (build_context _build/default)
   (library
    ((name alive)
     (uid b8a14a61f9163d57810cc01c7f39d084)
     (local true)
     (requires ())
     (source_dir _build/default)
     (modules
      (((name Alive)
        (impl (_build/default/alive.ml))
        (intf ())
        (cmt (_build/default/.alive.objs/byte/alive.cmt))
        (cmti ()))))
     (include_dirs (_build/default/.alive.objs/byte)))))
