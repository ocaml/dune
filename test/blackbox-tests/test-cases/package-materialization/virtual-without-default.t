A virtual library without a default implementation is still a valid compile-time
requirement. Computing a strict link closure must not prevent it from being
materialized.

  $ make_dune_project 3.24
  $ cat >>dune-project <<'EOF'
  > (package (name virtual-user))
  > (package (name virtual-api))
  > EOF

  $ mkdir api user
  $ cat >api/dune <<'EOF'
  > (library
  >  (name virtual_api)
  >  (public_name virtual-api)
  >  (virtual_modules virtual_api))
  > EOF
  $ echo 'val value : int' >api/virtual_api.mli

  $ cat >user/dune <<'EOF'
  > (library
  >  (name virtual_user)
  >  (public_name virtual-user)
  >  (modules ())
  >  (libraries virtual-api))
  > EOF

  $ cat >dune <<'EOF'
  > (rule
  >  (target result)
  >  (deps (package virtual-user))
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{bin:ocamlfind} query virtual-api))))
  > EOF

  $ dune build result
