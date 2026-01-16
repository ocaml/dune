Duplicate mld's in different scope
  $ dune build @doc-new

  $ dune trace cat | jq -c 'include "dune"; targetsMatching("page")'
  {"target_files":["_build/default/_doc_new/index/page-docs.odoc"]}
  {"target_files":["_build/default/_doc_new/index/stdlib/page-stdlib.odoc"]}
  {"target_files":["_build/default/_doc_new/index/local/page-local.odoc"]}
  {"target_files":["_build/default/_doc_new/index/local/scope2/page-scope2.odoc"]}
  {"target_files":["_build/default/_doc_new/index/local/scope1/page-scope1.odoc"]}
  {"target_files":["_build/default/_doc_new/odoc/local/scope2/page-foo.odoc"]}
  {"target_files":["_build/default/_doc_new/odoc/local/scope1/page-foo.odoc"]}
  {"target_files":["_build/default/_doc_new/index/stdlib/page-stdlib.odocl"]}
  {"target_files":["_build/default/_doc_new/index/local/page-local.odocl"]}
  {"target_files":["_build/default/_doc_new/index/page-docs.odocl"]}
  {"target_files":["_build/default/_doc_new/index/local/scope2/page-scope2.odocl"]}
  {"target_files":["_build/default/_doc_new/odoc/local/scope2/page-foo.odocl"]}
  {"target_files":["_build/default/_doc_new/index/local/scope1/page-scope1.odocl"]}
  {"target_files":["_build/default/_doc_new/odoc/local/scope1/page-foo.odocl"]}
