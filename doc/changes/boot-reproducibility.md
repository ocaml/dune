- Make bootstrap process reproducible independenly of `-j`.
  This can make the build slightly slower when Dune is built with `-j 1`.
  (#9563, fixes #9507, @emillon)
