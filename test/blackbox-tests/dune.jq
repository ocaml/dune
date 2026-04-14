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

def ruleTargets:
  (.targets.files // []) + (.targets.directories // []);

def pathMatches($path):
  . == $path or endswith("/" + $path);

def rulesMatchingTargetFilter(f):
  .[] | select((ruleTargets | any(f)));

def rulesMatchingTarget($path):
  rulesMatchingTargetFilter(pathMatches($path));

def depFileEntries:
  .[] | select(has("File")) | { kind: .File[0], path: .File[1] };

def depsFilePaths:
  depFileEntries | .path;

def depsFilePathsOfKind($kind):
  depFileEntries | select(.kind == $kind) | .path;

def depGlobEntries:
  .[] | select(has("glob")) | .glob
  | { dir_kind: .dir[0], dir: .dir[1], predicate, only_generated_files };

def depsGlobs:
  depGlobEntries;

def depsGlobEntriesWithPredicate($predicate):
  depGlobEntries | select(.predicate == $predicate);

def depsGlobDirsWithPredicate($predicate):
  depsGlobEntriesWithPredicate($predicate) | .dir;

def depsGlobPredicates:
  depGlobEntries | .predicate;

def ruleDepFileEntries:
  .deps | depFileEntries;

def ruleDepFilePaths:
  .deps | depsFilePaths;

def ruleDepFilePathsOfKind($kind):
  .deps | depsFilePathsOfKind($kind);

def ruleDepGlobEntries:
  .deps | depGlobEntries;

def ruleDepGlobDirsWithPredicate($predicate):
  .deps | depsGlobDirsWithPredicate($predicate);

def ruleDepGlobPredicates:
  .deps | depsGlobPredicates;

def ruleHasDepFile($path):
  [ ruleDepFilePaths | select(pathMatches($path)) ] | length > 0;

def ruleActionNodes:
  .action | .. | arrays | select(length > 0 and (.[0] | type) == "string");

def ruleActionsNamed($name):
  ruleActionNodes | select(.[0] == $name);

def ruleActionRunArgv:
  ruleActionsNamed("run") | .[1:];

def argListContainsSequence($seq):
  . as $args
  | ($seq | length) as $n
  | if $n == 0 then false
    elif ($args | length) < $n then false
    else [ range(0; ($args | length) - $n + 1) as $i
         | $args[$i:($i + $n)] == $seq
         ] | any
    end;

def argListHasSuffix($suffix):
  . as $args
  | ($suffix | length) as $n
  | if $n == 0 then false
    elif ($args | length) < $n then false
    else $args[(-$n):] == $suffix
    end;

def ruleHasRunArg($arg):
  [ ruleActionRunArgv | index($arg) != null ] | any;

def ruleHasRunContaining($seq):
  [ ruleActionRunArgv | argListContainsSequence($seq) ] | any;

def ruleHasRunSuffix($suffix):
  [ ruleActionRunArgv | argListHasSuffix($suffix) ] | any;

def ruleActionFlagValues($flag):
  ruleActionRunArgv as $args
  | ($args | length) as $n
  | if $n < 2
    then empty
    else range(0; $n - 1) | select($args[.] == $flag) | $args[. + 1]
    end;

def ruleHasCopy($src; $dst):
  [ ruleActionsNamed("copy") | select(.[1] == $src and .[2] == $dst) ]
  | length > 0;

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

def censorActionTargets:
  if has("target_files") then
    .target_files |= map(
      if test("\\.actions[/\\\\][^/\\\\]+[/\\\\][0-9a-f]{32}$") then
        sub("(?<sep>[/\\\\])[0-9a-f]{32}$"; "\(.sep)$ACTION")
      else . end)
  else . end;
