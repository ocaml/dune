Reproduce github issue #11058

Handling of more than one depopt:

  $ . ../helpers.sh

  $ mkpkg bar <<'EOF'
  > depopts: [ "a" "b" "c" ]
  > EOF

  $ solve bar
  Solution for dune.lock:
  - bar.0.0.1
