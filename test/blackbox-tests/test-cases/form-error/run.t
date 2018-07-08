we're getting an acceptable error message when adding a macro form in an
inappropariate place:

  $ dune build
  Info: creating file dune-project with this contents: (lang dune 1.0)
  /-----------------------------------------------------------------------
  | Internal error: Fiber.Execution_context.forward_error: error handler raised.
  | Invalid_argument("atom '%{read:x}' cannot be in dune syntax")
  | Raised at file "pervasives.ml", line 33, characters 20-45
  | Called from file "src/usexp/usexp.ml", line 26, characters 31-52
  | Called from file "list.ml", line 100, characters 12-15
  | Called from file "src/usexp/usexp.ml", line 36, characters 4-96
  | Called from file "list.ml", line 100, characters 12-15
  | Called from file "src/usexp/usexp.ml", line 36, characters 4-96
  | Called from file "format.ml", line 1288, characters 32-48
  | Called from file "format.ml", line 1337, characters 20-38
  | Called from file "src/report_error.ml", line 108, characters 4-12
  | Called from file "src/fiber/fiber.ml", line 243, characters 6-18
  | Re-raised at file "src/fiber/fiber.ml", line 39, characters 19-26
  | Called from file "src/fiber/fiber.ml", line 56, characters 6-20
  | 
  | Original exception was: Invalid_argument("atom '%{read:x}' cannot be in dune syntax")
  | Raised at file "pervasives.ml", line 33, characters 20-45
  | Called from file "src/usexp/usexp.ml", line 26, characters 31-52
  | Called from file "list.ml", line 100, characters 12-15
  | Called from file "src/usexp/usexp.ml", line 36, characters 4-96
  | Called from file "list.ml", line 100, characters 12-15
  | Called from file "src/usexp/usexp.ml", line 36, characters 4-96
  | Called from file "format.ml", line 1288, characters 32-48
  | Called from file "format.ml", line 1337, characters 20-38
  | Called from file "src/report_error.ml", line 108, characters 4-12
  | Called from file "src/fiber/fiber.ml", line 243, characters 6-18
  \-----------------------------------------------------------------------
  [1]
