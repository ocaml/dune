These opam variables are known as "switch variables" in opam, but since in Dune we don't
have switches, we consider them glboal variables. To keep inline with opam we consider
there to be a single switch named "dune" and all the installation locations should be in
the package building directories.

Note that misc is not supported in dune and a test for it can be found in
opam-var-unsupported.t
  $ mkrepo
  $ mkpkg testpkg <<EOF
  > build: [
  >   [ "echo" switch ]
  >   [ "echo" build ]
  >   [ "echo" prefix ]
  >   [ "echo" lib ]
  >   [ "echo" libexec ]
  >   [ "echo" bin ]
  >   [ "echo" sbin ]
  >   [ "echo" share ]
  >   [ "echo" doc ]
  >   [ "echo" etc ]
  >   [ "echo" man ]
  >   [ "echo" toplevel ]
  >   [ "echo" stublibs ]
  > ]
  > EOF
  > solve testpkg
  Solution for dune.lock:
  - testpkg.0.0.1
  $ cat ${default_lock_dir}/testpkg.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms
    ((action
      (progn
       (run echo %{switch})
       (run echo %{build})
       (run echo %{prefix})
       (run echo %{lib})
       (run echo %{libexec})
       (run echo %{bin})
       (run echo %{sbin})
       (run echo %{share})
       (run echo %{doc})
       (run echo %{etc})
       (run echo %{man})
       (run echo %{toplevel})
       (run echo %{stublibs}))))))

  $ build_pkg testpkg 2>&1 | censor
  dune
  $PWD/_build/.sandbox/$DIGEST/default/.pkg/testpkg
  $PWD/_build/.sandbox/$DIGEST/default/.pkg/testpkg/_install
  $PWD/_build/.sandbox/$DIGEST/default/.pkg/testpkg/_install/lib
  $PWD/_build/.sandbox/$DIGEST/default/.pkg/testpkg/_install/lib
  $PWD/_build/.sandbox/$DIGEST/default/.pkg/testpkg/_install/bin
  $PWD/_build/.sandbox/$DIGEST/default/.pkg/testpkg/_install/sbin
  $PWD/_build/.sandbox/$DIGEST/default/.pkg/testpkg/_install/share
  $PWD/_build/.sandbox/$DIGEST/default/.pkg/testpkg/_install/doc
  $PWD/_build/.sandbox/$DIGEST/default/.pkg/testpkg/_install/etc
  $PWD/_build/.sandbox/$DIGEST/default/.pkg/testpkg/_install/man
  $PWD/_build/.sandbox/$DIGEST/default/.pkg/testpkg/_install/lib/toplevel
  $PWD/_build/.sandbox/$DIGEST/default/.pkg/testpkg/_install/lib/stublibs
