- Fix handling of the `PATH` argument to `dune init proj NAME PATH`. An
  intermediate directory called `NAME` is no longer created if `PATH` is
  supplied, so `dune init proj my_project .` will now initialize a project in
  the current working directory. (#9447, fixes #9209, @shonfeder)
