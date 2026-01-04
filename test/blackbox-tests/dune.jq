def logs($m):
  select(.cat == "log" and (.args.message | contains($m))) | .args;

def redactCommandTimes:
  walk(if type == "object" then
    with_entries(
      if .key | IN("dur", "real", "user", "system") then
        .value = "redacted"
      else . end)
    else . end);

def targets: (.target_files // []) + (.target_dirs // []);

def targetsMatching($m):
    select(.cat == "process")
  | select(.args | targets | any(contains($m)))
  | .args
  | {target_files, target_dirs}
  | del(..|nulls);

def slowestCommands($n):
  [.[] | select(type == "object" and .args.commands != null)
       | .args.test as $test | .args.commands[] | {test: $test} + .]
  | sort_by(-.real)
  | .[:$n];
