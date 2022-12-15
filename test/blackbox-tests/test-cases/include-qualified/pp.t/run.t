We can set preprocessing options for nested modules
  $ dune build @all
  File "dune", line 8, characters 30-38:
  8 |      (run cat %{input-file})) bar/ppme))))
                                    ^^^^^^^^
  Error: "bar/ppme" is an invalid module name.
  Module names must be non-empty and composed only of the following characters:
  'A'..'Z', 'a'..'z', '_', ''' or '0'..'9'.
  Hint: barppme would be a correct module name
  [1]
