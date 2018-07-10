Bindings introduced by user dependencies should shadow existing bindings

  $ dune runtest
  Internal error, please report upstream including the contents of _build/log.
  Description:
  ("Local named variable not present in named deps"
   (pform "\%{read:y}")
   (deps_written_by_user ((:read (In_build_dir default/x)))))
  Backtrace:
  Raised at file "src/dep_path.ml" (inlined), line 45, characters 24-55
  Called from file "src/build_system.ml", line 89, characters 6-48
  Called from file "src/fiber/fiber.ml", line 243, characters 6-18
  foo
  [1]
