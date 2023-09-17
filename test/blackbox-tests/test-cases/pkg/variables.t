Test that we can set variables

  $ . ./helpers.sh

  $ mkdir dune.lock
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF
  $ cat >dune.lock/test.pkg <<EOF
  > (build
  >  (system "\| cat >test.config <<EOF
  >          "\| opam-version: "2.0"
  >          "\| variables {
  >          "\|   abool: true
  >          "\|   astring: "foobar"
  >          "\|   somestrings: ["foo" "bar"]
  >          "\| }
  >          "\| EOF
  >  ))
  > EOF

  $ cat >dune.lock/usetest.pkg <<EOF
  > (deps test)
  > (build
  >  (progn
  >   (system "\| echo %{pkg:test:abool}
  >           "\| echo %{pkg:test:astring}
  >           "\| echo %{pkg:test:somestrings}
  >   )
  >   (run mkdir -p %{prefix})))
  > EOF

  $ build_pkg usetest
  true
  foobar
  foo bar

  $ show_pkg_cookie test
  { files = map {}
  ; variables =
      [ ("abool", Bool true)
      ; ("astring", String "foobar")
      ; ("somestrings", Strings [ "foo"; "bar" ])
      ]
  }
