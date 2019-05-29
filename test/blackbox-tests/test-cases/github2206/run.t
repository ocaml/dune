copy_files would break the generation of the preprocessing flags
  $ dune build copy_files/.merlin
  File "copy_files/dune", line 5, characters 21-63:
  5 |  (preprocess (action (run %{project_root}/pp.exe %{input-file}))))
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning: .merlin generated is inaccurate. Cannot mix preprocessed and non preprocessed specificiations.
  Split the stanzas into different directories or silence this warning by adding (allow_approximate_merlin) to your dune-project.
  $ cat copy_files/.merlin | grep "FLG -pp"
  [1]
