Solving would add opam 'depext' field to lock directory packages

  $ . ../helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

Make a package for the library with depexts:
  $ mkpkg foo <<EOF
  > depexts: [["unzip" "gnupg"]]
  > EOF

Make a project that uses the foo library:
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name bar)
  >  (depends foo))
  > EOF

locking would add the opam 'depext' field to foo.pkg
  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - foo.0.0.1
  $ cat ${default_lock_dir}/foo.0.0.1.pkg
  (version 0.0.1)
  
  (depexts
   (unzip gnupg))
