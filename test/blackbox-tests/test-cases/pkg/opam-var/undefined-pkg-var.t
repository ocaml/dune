Variables referencing packages not in the lock evaluate to empty string,
matching opam semantics.

  $ mkrepo

  $ mkpkg "in-lock"

  $ mkpkg "test-pkg" <<'EOF'
  > depends: [ "in-lock" ]
  > build: [
  >   [ "sh" "-c" "echo 'in-lock:[%{in-lock:version}%]'" ]
  >   [ "sh" "-c" "echo 'not-in-lock:[%{not-in-lock:version}%]'" ]
  > ]
  > EOF

  $ solve test-pkg
  Solution for dune.lock:
  - in-lock.0.0.1
  - test-pkg.0.0.1

  $ build_pkg test-pkg
  File "dune.lock/test-pkg.0.0.1.pkg", line 8, characters 16-65:
  8 |      (run sh -c "echo 'not-in-lock:[%{pkg:not-in-lock:version}]'"))))))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Undefined package variable "version"
  [1]
