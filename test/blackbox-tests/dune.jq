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

def processes: select(.cat == "process" and .name == "finish");

def targetsMatching($m):
    processes
  | select(.args | targets | any(contains($m)))
  | .args
  | {target_files, target_dirs}
  | del(..|nulls);

def slowestCommands($n):
  [.[] | select(type == "object" and .args.commands != null)
       | .args.test as $test | .args.commands[] | {test: $test} + .]
  | sort_by(-.real)
  | .[:$n];

def basename: split("/") | last;

def rocqArg:
    sub(".*/coq/theories"; "coq/theories")
  | sub(".*/rocq-runtime/"; "rocq-runtime/")
  | sub(".*/coq/"; "coq/")
  | sub(".*/coq-core"; "coq-core");

def rocqFlags:
    processes
  | select(.args.prog | basename | (contains("rocq") or contains("coq")))
  | {name: (.args.prog | basename), args: (.args.process_args | map(rocqArg))};

def coqcCoqdepFlags:
    processes
  | select(.args.process_args[0] | IN("compile", "dep"))
  | {name: .args.prog | basename, args: (.args.process_args | map(rocqArg))};

def coqdocFlags:
    processes
  | select((.args.prog | basename) == "coqdoc")
  | .args.process_args
  | .[]
  | rocqArg;

def redactedActionTraces:
  [ .[] | select(.args.digest != null) | .ts |= 0 | .args.digest |= "REDACTED" ] | sort_by(.name) | .[];

def cacheEvent($path):
  select(.cat == "cache") | .args | select(.path == $path);

def cacheMisses:
  select(.cat == "cache") | select(.name == "workspace_local_miss" or .name == "miss");

def cacheMissesMatching($path):
    [ .[] | cacheMisses ]
  | map(select((.args.target // .args.head) | test("source|target1")))
  | sort_by(.args.target // .args.head)
  | .[] | {name, target: (.args.target // .args.head), reason: .args.reason};

def fsUpdateWithPath($path):
    select(.name == "fs_update")
  | .args
  | select(.path == $path);
