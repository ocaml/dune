The project "root_project" vendor "a" and "b" project.
To override "a" project in "b", we use "data_only_dirs":
"(subdir b (data_only_dirs a))"

  $ dune build
