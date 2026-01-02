# Count how many times a command with the given argument appears in the trace.
# Useful for verifying whether a build action ran (count > 0) or was cached (count = 0).
count_trace() {
  dune trace cat \
    | jq -s --arg needle "$1" '[ .[] | select(.cat == "process" and .args.process_args == [$needle]) ] | length'
}
