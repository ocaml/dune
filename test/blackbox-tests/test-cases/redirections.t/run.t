  $ dune runtest

  $ dune trace cat | jq -c '
  > include "dune";
  >   progMatchingFilter(. == "sh" or . == "diff")
  > | del(.target_files)
  > '
  {"prog":"sh","process_args":["-c","echo toto"]}
  {"prog":"sh","process_args":["-c","echo toto"]}
  {"prog":"sh","process_args":["-c","echo titi >&2"]}
  {"prog":"sh","process_args":["-c","echo titi >&2"]}
  {"prog":"diff","process_args":["-uw","stdout.expected","stdout"]}
  {"prog":"diff","process_args":["-uw","stderr.expected","stderr"]}
  {"prog":"diff","process_args":["-uw","both.expected","both"]}
