- boot: remove single-command bootstrap. This was an alternative bootstrap
  strategy that was used in certain conditions. Removal makes the bootstrap a
  bit slower on Linux when only a single core is available, but bootstrap is
  now reproducible in all cases. (#9735, fixes #9507, @emillon)
