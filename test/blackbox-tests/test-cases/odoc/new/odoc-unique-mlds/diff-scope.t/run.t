Duplicate mld's in different scope
  $ index_dir=_build/default/_doc_new/index
  $ index_local_dir="$index_dir/local"
  $ odoc_local_dir=_build/default/_doc_new/odoc/local
  $ dune build \
  >   "$index_dir/page-docs.odoc" \
  >   "$index_dir/stdlib/page-stdlib.odoc" \
  >   "$index_local_dir/page-local.odoc" \
  >   "$index_local_dir/scope2/page-scope2.odoc" \
  >   "$index_local_dir/scope1/page-scope1.odoc" \
  >   "$odoc_local_dir/scope2/page-foo.odoc" \
  >   "$odoc_local_dir/scope1/page-foo.odoc" \
  >   "$index_dir/stdlib/page-stdlib.odocl" \
  >   "$index_local_dir/page-local.odocl" \
  >   "$index_dir/page-docs.odocl" \
  >   "$index_local_dir/scope2/page-scope2.odocl" \
  >   "$odoc_local_dir/scope2/page-foo.odocl" \
  >   "$index_local_dir/scope1/page-scope1.odocl" \
  >   "$odoc_local_dir/scope1/page-foo.odocl"

  $ dune trace cat | jq_dune -c 'targetsMatching("page")'
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
