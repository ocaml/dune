This test checks that there is no clash when two private libraries have the same name

  $ dune build @doc-new
  File "page-b.odoc":
  Warning: Failed to lookup child page dummy
  File "page-a.odoc":
  Warning: Failed to lookup child page dummy

  $ dune trace cat | jq -c 'include "dune"; targetsMatching("docs/test")'
  {"target_dirs":["_build/default/_doc_new/html/docs/test@38f98c954b37/Test"]}
  {"target_files":["_build/default/_doc_new/html/docs/test@38f98c954b37/index.html"]}
