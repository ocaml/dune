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

def targetsMatchingFilter(f):
    processes
  | select(.args | targets | any(f))
  | .args
  | {target_files, target_dirs}
  | del(..|nulls);

def targetsMatching($m):
    processes
  | select(.args | targets | any(contains($m)))
  | .args
  | {target_files, target_dirs}
  | del(..|nulls);

def basename: split("/") | last;

def progMatching($m):
    processes
  | select(.args | .prog | contains($m))
  | .args
  | {prog : (.prog | basename), process_args, target_files, target_dirs}
  | del(..|nulls);

def progMatchingFilter(f):
    processes
  | select(.args | .prog | basename | f)
  | .args
  | {prog : (.prog | basename), process_args, target_files, target_dirs}
  | del(..|nulls);

def slowestCommands($n):
  [.[] | select(type == "object" and .args.commands != null)
       | .args.test as $test | .args.commands[] | {test: $test} + .]
  | sort_by(-.real)
  | .[:$n];

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
  | select((.args.prog | basename | (contains("rocq") or contains("coq"))) and .args.process_args[0] == "doc")
  | .args.process_args[1:]
  | .[]
  | rocqArg;

def merlinEntry($module_name):
  .[] | select(.module_name == $module_name);

def merlinBuildPath:
  "_build/\(.source_path)";

def merlinPathSummary:
  { module_name, source_path: merlinBuildPath };

def merlinPathLine:
  "\(.module_name): \(merlinBuildPath)";

def merlinConfigItems:
  .config[];

def merlinConfigItemsNamed($names):
  .config[] | select(.[0] as $name | $names | index($name));

def merlinJsonEntry:
  merlinPathLine,
  (merlinConfigItems | @json);

def merlinJsonEntryWithConfigNames($names):
  merlinPathLine,
  (merlinConfigItemsNamed($names) | @json);

def merlinUnitName:
  first(.config[] | select(.[0] == "UNIT_NAME") | .[1]);

def merlinUnitNameSummary:
  merlinPathSummary + { unit_name: merlinUnitName };

def merlinConfigSummary($names):
  merlinPathSummary + { config: [merlinConfigItemsNamed($names)] };

def redactedActionTraces:
  [ .[]
  | select(.cat != "config" and .args.digest != null)
  | .ts |= 0
  | .args.digest
  |= "REDACTED"
  ] | sort_by(.name) | .[];

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

def censorDigestDir:
  .args.dir |= (if . then sub("[0-9a-f]{32}"; "$DIGEST") else . end);
