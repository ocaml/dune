This test checks that there is no clash when two private libraries have the same name

  $ dune build @doc-new
  File "page-b.odoc":
  Warning: Failed to lookup child page dummy
  File "page-a.odoc":
  Warning: Failed to lookup child page dummy

  $ dune trace cat \
  > | jq -c 'include "dune"; targetsMatching("docs/test")' \
  > | dune_cmd subst 'test@[a-f0-9]+/' 'test@$DIGEST/'
  {"target_dirs":["_build/default/_doc_new/html/docs/test@$DIGEST/Test"]}
  {"target_files":["_build/default/_doc_new/html/docs/test@$DIGEST/index.html"]}
