Reproduce github issue #11058

Handling of more than one depopt:

  $ . ../helpers.sh

  $ mkpkg a
  $ mkpkg b
  $ mkpkg c
  $ mkpkg d
  $ mkpkg e
  $ mkpkg f

  $ runtest() {
  > mkpkg bar <<'EOF'
  > depopts: [ "a" "b" "c" ]
  > EOF
  > solve bar
  > }

  $ runtest <<'EOF'
  > depopts: [ "a" "b" "c" ]
  > EOF
  Solution for .dune-solution-cache:
  - bar.0.0.1

  $ runtest <<'EOF'
  > depopts: [ "a" "b" "c" "d" ]
  > EOF
  Solution for .dune-solution-cache:
  - bar.0.0.1

  $ runtest <<'EOF'
  > depopts: [ ("a" | "b") "c" "d" ]
  > EOF
  Solution for .dune-solution-cache:
  - bar.0.0.1

  $ runtest <<'EOF'
  > depopts: [ (("e" | "a") | ("d" | "f")) "b" "c" ]
  > EOF
  Solution for .dune-solution-cache:
  - bar.0.0.1
