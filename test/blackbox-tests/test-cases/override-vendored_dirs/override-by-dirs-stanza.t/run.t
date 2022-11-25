The project "root_project" vendor "a" and "b" project.
To override "a" project in "b", we use "dirs":
"(subdir b (dirs :standard \ a))"

  $ dune build
