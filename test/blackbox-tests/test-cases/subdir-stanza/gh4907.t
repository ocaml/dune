
  $ cat <<EOF >dune-project
  > (lang dune 3.0)
  > EOF
  $ cat <<EOF >dune
  > (subdir "foo bar" (rule with-stout-to (echo foo) ./foo))
  > EOF
  $ dune build
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Dune_lang.Atom.of_string got invalid atom", { atom = "foo bar" })
  Raised at Stdune__Code_error.raise in file
    "otherlibs/stdune-unstable/code_error.ml", line 11, characters 30-62
  Called from Dune_engine__Sub_dirs.decode_includes.subdir.(fun) in file
    "src/dune_engine/sub_dirs.ml", line 316, characters 22-48
  Called from Dune_lang__Decoder.next in file "src/dune_lang/decoder.ml"
    (inlined), line 282, characters 22-28
  Called from Dune_lang__Decoder.plain_string in file
    "src/dune_lang/decoder.ml", line 339, characters 2-208
  Called from Dune_lang__Decoder.(>>=) in file "src/dune_lang/decoder.ml", line
    137, characters 17-28
  Called from Dune_lang__Decoder.multi_field.loop in file
    "src/dune_lang/decoder.ml", line 666, characters 25-39
  Called from Dune_lang__Decoder.multi_field in file
    "src/dune_lang/decoder.ml", line 669, characters 12-55
  Called from Dune_lang__Decoder.and+ in file "src/dune_lang/decoder.ml", line
    153, characters 17-28
  Called from Dune_lang__Decoder.(>>|) in file "src/dune_lang/decoder.ml", line
    141, characters 17-28
  Called from Dune_lang__Decoder.fields in file "src/dune_lang/decoder.ml",
    line 674, characters 21-53
  Called from Dune_lang__Decoder.enter.(fun) in file
    "src/dune_lang/decoder.ml", line 368, characters 19-28
  Called from Dune_lang__Decoder.next_with_user_context in file
    "src/dune_lang/decoder.ml" (inlined), line 288, characters 22-51
  Called from Dune_lang__Decoder.enter in file "src/dune_lang/decoder.ml", line
    364, characters 2-242
  Called from Dune_lang__Decoder.(>>=) in file "src/dune_lang/decoder.ml", line
    137, characters 17-28
  Called from Dune_lang__Decoder.parse in file "src/dune_lang/decoder.ml", line
    255, characters 13-29
  Called from Dune_engine__Source_tree.Dune_file.load_plain in file
    "src/dune_engine/source_tree.ml", line 89, characters 8-103
  Called from Dune_engine__Source_tree.Dune_file.load.(fun) in file
    "src/dune_engine/source_tree.ml", line 111, characters 22-66
  Called from Stdune__Exn.protectx in file "otherlibs/stdune-unstable/exn.ml",
    line 12, characters 8-11
  Re-raised at Stdune__Exn.protectx in file "otherlibs/stdune-unstable/exn.ml",
    line 18, characters 4-11
  Called from Fiber.O.(>>|).(fun) in file "src/fiber/fiber.ml", line 288,
    characters 36-41
  Called from Fiber.Execution_context.apply in file "src/fiber/fiber.ml", line
    182, characters 9-14
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune-unstable/exn.ml", line 36, characters 27-56
  Called from Fiber.Execution_context.run_jobs in file "src/fiber/fiber.ml",
    line 204, characters 8-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune-unstable/exn.ml", line 36, characters 27-56
  Called from Fiber.Execution_context.run_jobs in file "src/fiber/fiber.ml",
    line 204, characters 8-13
  Re-raised at Stdune__Exn.raise_with_backtrace in file
    "otherlibs/stdune-unstable/exn.ml", line 36, characters 27-56
  Called from Fiber.Execution_context.run_jobs in file "src/fiber/fiber.ml",
    line 204, characters 8-13
  -> required by ("find-dir-raw", ".")
  -> required by ("<unnamed>", ())
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]
