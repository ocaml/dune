Variables referencing packages not in the lock evaluate to empty string at
solve time, matching opam semantics.

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

The lock file shows that the unavailable package variable is resolved to empty
string at solve time:

  $ cat dune.lock/test-pkg.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms
    ((action
      (progn
       (run sh -c "echo 'in-lock:[%{pkg:in-lock:version}]'")
       (run sh -c "echo 'not-in-lock:[]'"))))))
  
  (depends
   (all_platforms (in-lock)))




