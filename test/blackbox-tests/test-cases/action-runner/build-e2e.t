We build a project by delegating some build commands to action runners

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > EOF

ar1 and ar2 will be built by action runners. While self will be built by the
dune command
  $ mkdir ar1 ar2 self

  $ echo "(rule (with-stdout-to aaa (echo xxx)))" > ar1/dune
  $ echo "(rule (with-stdout-to bbb (echo yyy)))" > ar2/dune
  $ echo "(rule (with-stdout-to ccc (echo zzz)))" > self/dune

  $ timeout 2 dune internal action-runner build --runner ar1 --runner ar2
  $ ar1="_build/ar1.*.log"
  $ grep aaa $ar1
  # mkdir -p _build/default/ar1;cd _build/default/ar1;echo -n xxx > aaa
  $ ar2="_build/ar2.*.log"
  $ grep bbb $ar2
  # mkdir -p _build/default/ar2;cd _build/default/ar2;echo -n yyy > bbb
