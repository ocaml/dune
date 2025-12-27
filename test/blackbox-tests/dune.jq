def logs($m):
  select(.cat == "log" and (.args.message | contains($m))) | .args;
