Test that we can set variables

  $ mkdir dune.lock
  $ cat >dune.lock/test <<EOF
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

  $ cat >dune.lock/usetest <<EOF
  > (deps test)
  > (build
  >  (progn
  >   (system "\| echo %{pkg:var:test:abool}
  >           "\| echo %{pkg:var:test:astring}
  >           "\| echo %{pkg:var:test:somestrings}
  >   )
  >   (run mkdir -p %{prefix})))
  > EOF

  $ dune build .pkg/usetest/target/
  true
  foobar
  foo bar

  $ dune internal dump _build/default/.pkg/test/target/cookie
  { files = map {}
  ; variables =
      [ ("abool", Bool true)
      ; ("astring", String "foobar")
      ; ("somestrings", Strings [ "foo"; "bar" ])
      ]
  }
