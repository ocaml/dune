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
  $PWD/_build/.sandbox/$DIGEST1/_private/default/.pkg/testpkg.0.0.1-$DIGEST2/source
  $PWD/_build/.sandbox/$DIGEST1/_private/default/.pkg/testpkg.0.0.1-$DIGEST2/target
  $PWD/_build/.sandbox/$DIGEST1/_private/default/.pkg/testpkg.0.0.1-$DIGEST2/target/lib
  $PWD/_build/.sandbox/$DIGEST1/_private/default/.pkg/testpkg.0.0.1-$DIGEST2/target/lib
  $PWD/_build/.sandbox/$DIGEST1/_private/default/.pkg/testpkg.0.0.1-$DIGEST2/target/bin
  $PWD/_build/.sandbox/$DIGEST1/_private/default/.pkg/testpkg.0.0.1-$DIGEST2/target/sbin
  $PWD/_build/.sandbox/$DIGEST1/_private/default/.pkg/testpkg.0.0.1-$DIGEST2/target/share
  $PWD/_build/.sandbox/$DIGEST1/_private/default/.pkg/testpkg.0.0.1-$DIGEST2/target/doc
  $PWD/_build/.sandbox/$DIGEST1/_private/default/.pkg/testpkg.0.0.1-$DIGEST2/target/etc
  $PWD/_build/.sandbox/$DIGEST1/_private/default/.pkg/testpkg.0.0.1-$DIGEST2/target/man
  $PWD/_build/.sandbox/$DIGEST1/_private/default/.pkg/testpkg.0.0.1-$DIGEST2/target/lib/toplevel
  $PWD/_build/.sandbox/$DIGEST1/_private/default/.pkg/testpkg.0.0.1-$DIGEST2/target/lib/stublibs
