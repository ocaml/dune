  $ cp ../bin/foo.exe ./
  $ dune runtest --display short
           foo alias runtest (exit 1)
  (cd _build/default && ./foo.exe)
  bar is written despite not being declared as a target in dune file. To fix, add it to target list in dune file.
  [1]
