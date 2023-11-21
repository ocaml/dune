When `-x` is passed and a dune file in OCaml syntax is loaded, we should not
crash.
See #9239.

  $ cat > dune-project << EOF
  > (lang dune 1.0)
  > EOF

  $ cat > dune << EOF
  > (* -*- tuareg -*- *)
  > let () = Jbuild_plugin.V1.send ""
  > EOF

  $ dune build -x cross 2>&1 | grep -A2 -e Internal -e 'required by'
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("List.find_exn", {})
  --
  -> required by ("context-db-get", "cross")
  -> required by ("Script.eval_one", <opaque>)
  -> required by ("make_sctx", ())
  -> required by ("Super_context.all", ())
  -> required by ("toplevel", ())
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
