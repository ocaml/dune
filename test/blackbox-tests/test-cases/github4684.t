Combining `(root_module ...)` with a public library results in an unexpected
build failure:

  $ echo "module X = Root.Unix" > main.ml
  $ touch main.opam
  $ cat >dune <<EOF
  > (library
  >  (name main)
  >  (public_name main)
  >  (root_module root)
  >  (libraries unix))
  > EOF

  $ dune build
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("External.cm_dir",
    { t =
        { public_dir = In_build_dir "default"
        ; private_dir = None
        ; public_cmi_dir = None
        }
    })
  Raised at file "otherlibs/stdune-unstable/code_error.ml", line 11, characters
    30-62
  Called from file "src/dune_rules/install_rules.ml", line 148, characters
    19-69
  Called from file "src/dune_rules/install_rules.ml" (inlined), line 165,
    characters 23-31
  Called from file "src/dune_rules/install_rules.ml", line 180, characters
    49-63
  Called from file "list.ml", line 103, characters 22-25
  Called from file "otherlibs/stdune-unstable/list.ml", line 5, characters
    19-33
  Called from file "list.ml", line 103, characters 22-25
  Called from file "otherlibs/stdune-unstable/list.ml" (inlined), line 5,
    characters 19-33
  Called from file "otherlibs/stdune-unstable/list.ml", line 40, characters
    29-39
  Called from file "src/dune_rules/install_rules.ml", line 155, characters
    6-1023
  Called from file "src/fiber/fiber.ml", line 182, characters 9-14
  Re-raised at file "otherlibs/stdune-unstable/exn.ml", line 36, characters
    27-56
  Called from file "src/fiber/fiber.ml", line 204, characters 8-13
  Re-raised at file "otherlibs/stdune-unstable/exn.ml", line 36, characters
    27-56
  Called from file "src/fiber/fiber.ml", line 204, characters 8-13
  Re-raised at file "otherlibs/stdune-unstable/exn.ml", line 36, characters
    27-56
  Called from file "src/fiber/fiber.ml", line 204, characters 8-13
  Re-raised at file "otherlibs/stdune-unstable/exn.ml", line 36, characters
    27-56
  Called from file "src/fiber/fiber.ml", line 204, characters 8-13
  Re-raised at file "otherlibs/stdune-unstable/exn.ml", line 36, characters
    27-56
  Called from file "src/fiber/fiber.ml", line 204, characters 8-13
  Re-raised at file "otherlibs/stdune-unstable/exn.ml", line 36, characters
    27-56
  Called from file "src/fiber/fiber.ml", line 204, characters 8-13
  Re-raised at file "otherlibs/stdune-unstable/exn.ml", line 36, characters
    27-56
  Called from file "src/fiber/fiber.ml", line 204, characters 8-13
  Re-raised at file "otherlibs/stdune-unstable/exn.ml", line 36, characters
    27-56
  Called from file "src/fiber/fiber.ml", line 204, characters 8-13
  Re-raised at file "otherlibs/stdune-unstable/exn.ml", line 36, characters
    27-56
  Called from file "src/fiber/fiber.ml", line 472, characters 10-22
  Called from file "src/fiber/fiber.ml", line 127, characters 10-17
  Re-raised at file "otherlibs/stdune-unstable/exn.ml", line 36, characters
    27-56
  Called from file "src/fiber/fiber.ml", line 204, characters 8-13
  Re-raised at file "otherlibs/stdune-unstable/exn.ml", line 36, characters
    27-56
  Called from file "src/fiber/fiber.ml", line 204, characters 8-13
  -> required by ("stanzas-to-entries", "default")
  -> required by ("<unnamed>", ())
  -> required by ("<unnamed>", ())
  -> required by ("<unnamed>", ())
  -> required by ("<unnamed>", ())
  -> required by ("load-dir", In_build_dir "default")
  -> required by ("build-alias", { dir = "default"; name = "default" })
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]

