Here we test the translation and implementation of opam package variables that
we intend to interpret in dune.

  $ mkrepo
  > mkpkg "testpkg" <<'EOF'
  > depends: [ "foo" ]
  > build: [
  >   [ "echo"     name ]
  >   [ "echo"   _:name ]
  >   [ "echo" foo:name ]
  >   [ "echo"     version ]
  >   [ "echo"   _:version ]
  >   [ "echo" foo:version ]
  >   [ "echo"    _:installed ]
  >   [ "echo"  foo:installed ]
  >   [ "echo"   _:enable ]
  >   [ "echo" foo:enable ]
  >   [ "echo"   _:pinned ]
  >   [ "echo" foo:pinned ]
  >   [ "echo"   _:bin ]
  >   [ "echo" foo:bin ]
  >   [ "echo"   _:sbin ]
  >   [ "echo" foo:sbin ]
  >   [ "echo"   _:lib ]
  >   [ "echo" foo:lib ]
  >   [ "echo"   _:man ]
  >   [ "echo" foo:man ]
  >   [ "echo"   _:doc ]
  >   [ "echo" foo:doc ]
  >   [ "echo"   _:share ]
  >   [ "echo" foo:share ]
  >   [ "echo"   _:etc ]
  >   [ "echo" foo:etc ]
  >   [ "echo"   _:dev ]
  >   [ "echo" foo:dev ]
  >   [ "echo"   _:build-id ]
  >   [ "echo" foo:build-id ]
  >   [ "echo"     with-test ]
  >   [ "echo"   _:with-test ]
  >   [ "echo" foo:with-test ]
  >   [ "echo"     with-doc ]
  >   [ "echo"   _:with-doc ]
  >   [ "echo" foo:with-doc ]
  >   [ "echo"   _:with-dev-setup ]
  >   [ "echo" foo:with-dev-setup ]
  > ]
  > EOF
  > mkpkg "foo" <<EOF
  > EOF
  > solve testpkg
  Solution for dune.lock:
  - foo.0.0.1
  - testpkg.0.0.1

Inspecting the lockfile we can see how each opam package variable was
translated into a corresponding Dune version.

  $ cat ${default_lock_dir}/testpkg.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms
    ((action
      (progn
       (run echo %{pkg-self:name})
       (run echo %{pkg-self:name})
       (run echo %{pkg:foo:name})
       (run echo %{pkg-self:version})
       (run echo %{pkg-self:version})
       (run echo %{pkg:foo:version})
       (run echo %{pkg-self:installed})
       (run echo %{pkg:foo:installed})
       (run
        echo
        (if (catch_undefined_var %{pkg-self:installed} false) enable disable))
       (run
        echo
        (if (catch_undefined_var %{pkg:foo:installed} false) enable disable))
       (run echo %{pkg-self:pinned})
       (run echo %{pkg:foo:pinned})
       (run echo %{pkg-self:bin})
       (run echo %{pkg:foo:bin})
       (run echo %{pkg-self:sbin})
       (run echo %{pkg:foo:sbin})
       (run echo %{pkg-self:lib})
       (run echo %{pkg:foo:lib})
       (run echo %{pkg-self:man})
       (run echo %{pkg:foo:man})
       (run echo %{pkg-self:doc})
       (run echo %{pkg:foo:doc})
       (run echo %{pkg-self:share})
       (run echo %{pkg:foo:share})
       (run echo %{pkg-self:etc})
       (run echo %{pkg:foo:etc})
       (run echo %{pkg-self:dev})
       (run echo %{pkg:foo:dev})
       (run echo %{pkg-self:build-id})
       (run echo %{pkg:foo:build-id})
       (run echo %{pkg-self:with-test})
       (run echo %{pkg-self:with-test})
       (run echo %{pkg:foo:with-test})
       (run echo %{pkg-self:with-doc})
       (run echo %{pkg-self:with-doc})
       (run echo %{pkg:foo:with-doc})
       (run echo %{pkg-self:with-dev-setup})
       (run echo %{pkg:foo:with-dev-setup}))))))
  
  (depends
   (all_platforms (foo)))

The values here are not important, but Dune should be able to interpret the
variables.

  $ build_pkg testpkg
  File "dune.lock/testpkg.0.0.1.pkg", line 41, characters 15-36:
  41 |      (run echo %{pkg-self:with-test})
                      ^^^^^^^^^^^^^^^^^^^^^
  Error: Undefined package variable "with-test"
  File "dune.lock/testpkg.0.0.1.pkg", line 42, characters 15-36:
  42 |      (run echo %{pkg-self:with-test})
                      ^^^^^^^^^^^^^^^^^^^^^
  Error: Undefined package variable "with-test"
  File "dune.lock/testpkg.0.0.1.pkg", line 43, characters 15-35:
  43 |      (run echo %{pkg:foo:with-test})
                      ^^^^^^^^^^^^^^^^^^^^
  Error: Undefined package variable "with-test"
  File "dune.lock/testpkg.0.0.1.pkg", line 44, characters 15-35:
  44 |      (run echo %{pkg-self:with-doc})
                      ^^^^^^^^^^^^^^^^^^^^
  Error: Undefined package variable "with-doc"
  File "dune.lock/testpkg.0.0.1.pkg", line 45, characters 15-35:
  45 |      (run echo %{pkg-self:with-doc})
                      ^^^^^^^^^^^^^^^^^^^^
  Error: Undefined package variable "with-doc"
  File "dune.lock/testpkg.0.0.1.pkg", line 46, characters 15-34:
  46 |      (run echo %{pkg:foo:with-doc})
                      ^^^^^^^^^^^^^^^^^^^
  Error: Undefined package variable "with-doc"
  File "dune.lock/testpkg.0.0.1.pkg", line 47, characters 15-41:
  47 |      (run echo %{pkg-self:with-dev-setup})
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Undefined package variable "with-dev-setup"
  File "dune.lock/testpkg.0.0.1.pkg", line 48, characters 15-40:
  48 |      (run echo %{pkg:foo:with-dev-setup}))))))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Undefined package variable "with-dev-setup"
  [1]
