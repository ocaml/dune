We test depopts with conflicting constraints to see which one the solver will
prefer if any:

  $ . ../helpers.sh
  $ mkpkg foo 1
  $ mkpkg foo 2

  $ mkpkg bar <<'EOF'
  > depopts: [ "foo" {= "1"} ]
  > EOF

  $ mkpkg baz <<'EOF'
  > depopts: [ "foo" {= "2"} ]
  > EOF

We don't currently support depopts so they are both omitted.

  $ solve bar baz
  Solution for dune.lock:
  - bar.0.0.1
  - baz.0.0.1

It's possible to find a solution by satisfying one (but not both) depopts.
