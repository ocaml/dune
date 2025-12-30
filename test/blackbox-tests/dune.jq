def logs($m):
  select(.cat == "log" and (.args.message | contains($m))) | .args;

def redactCommandTimes:
  walk(if type == "object" then
    with_entries(
      if .key | IN("dur", "real", "user", "system") then
        .value = "redacted"
      else . end)
    else . end);
