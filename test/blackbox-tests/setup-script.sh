jq() {
  command jq -L"$INSIDE_DUNE"/test/blackbox-tests "$@"
}

jq_dune() {
  local args=()
  local filter_set=0

  while [ "$#" -gt 0 ]
  do
    case "$1" in
      -L|-f|--from-file)
        args+=("$1" "$2")
        shift 2
        ;;
      --arg|--argjson|--slurpfile|--rawfile|--argfile)
        args+=("$1" "$2" "$3")
        shift 3
        ;;
      --)
        args+=("$1")
        shift
        ;;
      -*)
        args+=("$1")
        shift
        ;;
      *)
        if [ "$filter_set" -eq 0 ]
        then
          args+=("include \"dune\"; $1")
          filter_set=1
        else
          args+=("$1")
        fi
        shift
        ;;
    esac
  done

  jq "${args[@]}"
}

is_linked() {
  nlinks=$(dune_cmd stat hardlinks "$1")
  [ "$nlinks" -gt 1 ] && echo linked || echo not linked
}

export XDG_CACHE_HOME="$PWD/.cache"

setup_xdg_runtime_dir () {
    export XDG_RUNTIME_DIR="${TMPDIR:-$PWD}/.xdg-runtime"
    mkdir -p "$XDG_RUNTIME_DIR"
    chmod 700 "$XDG_RUNTIME_DIR"
}

setup_basic_shared_cache_project () {
    local mode="$1"

    cat > config <<EOF
(lang dune 3.0)
(cache enabled)
EOF
    if [ "$mode" != "default" ]; then
        cat >> config <<EOF
(cache-storage-mode $mode)
EOF
    fi
    cat > dune-project <<EOF
(lang dune 3.5)
EOF
    cat > dune <<'EOF'
(rule
 (deps source)
 (targets target1 target2)
 (action
  (progn
   (no-infer (with-stdout-to beacon (echo "")))
   (with-stdout-to target1 (cat source))
   (with-stdout-to target2 (cat source source)))))
EOF
    cat > source <<'EOF'
\_o< COIN
EOF
}

init_oxcaml_project() {
  cat > "dune-project" <<- EOF
	(lang dune 3.20)
	(using oxcaml 0.1)
	EOF
}

make_dune_project() {
  version="$1"
  cat > dune-project <<- EOF
	(lang dune $version)
	EOF
}

make_mypkg_bin_project() {
  local message="${1:-hello from mybin}"
  cat > dune-project <<- EOF
	(lang dune 3.24)
	(package (name mypkg))
	EOF
  mkdir src
  cat > src/dune <<-'EOF'
	(executable (public_name mybin) (package mypkg))
	EOF
  cat > src/mybin.ml <<- EOF
	let () = print_endline "$message"
	EOF
}

make_mypkg_lib_project() {
  make_dune_project 3.24
  cat >> dune-project <<-'EOF'
	(package (name mypkg))
	EOF
  mkdir src
  cat > src/dune <<-'EOF'
	(library (public_name mypkg))
	EOF
  cat > src/mypkg.ml <<-'EOF'
	let x = 1
	EOF
}

make_directory_targets_project() {
  local version="${1:-3.23}"
  cat > dune-project <<- EOF
	(lang dune ${version})
	(using directory-targets 0.1)
	EOF
}

make_simple_rpc_watch_project() {
  make_dune_project 3.23
  cat > dune <<-'EOF'
	(rule
	 (target x)
	 (action (write-file %{target} ok)))
	EOF
}

odoc_detect_syntax() {
  if grep -q '>sig<' "$1"
  then
    echo it is ocaml
  elif grep -q '{ ... }' "$1"
  then
    echo it is reason
  else
    echo it is unknown
  fi
}

query_ocaml_merlin() {
  file="$1"
  shift
  query=$(mktemp "${TMPDIR:-.}/merlin-query.XXXXXX")
  printf '(File "%s")\n' "$file" | dune internal sexp-to-csexp > "$query"
  dune ocaml-merlin "$@" < "$query"
  rm -f "$query"
}

query_ocaml_merlin_pp() {
  output=$(mktemp "${TMPDIR:-.}/merlin-output.XXXXXX")
  query_ocaml_merlin "$@" > "$output"
  dune internal sexp-pp --format=csexp "$output"
  rm -f "$output"
}

query_ocaml_merlin_sexp() {
  output=$(mktemp "${TMPDIR:-.}/merlin-output.XXXXXX")
  query_ocaml_merlin "$@" > "$output"
  dune internal sexp-pp --format=csexp --compact "$output"
  rm -f "$output"
}

make_dir_with_dune() {
  path="$1"
  mkdir -p "$path"
  cat > "$path/dune"
}

make_dummy_intf() {
  dir="$1"
  name="$2"
  cat >> "$dir/$name.mli" <<- EOF
	type t
	val f : t -> unit
	EOF
}

make_dummy_impl() {
  dir="$1"
  name="$2"
  cat >> "$dir/$name.ml" <<- EOF
	type t = int
	let f _ = ()
	EOF
}

make_lib_impl() {
  name="$1"
  implements="$2"
  make_dir_with_dune "$name" <<- EOF
	(library
	  (name $name)
	  (implements $implements))
	EOF
}

target_cmi() {
  echo "./$1/.$1.objs/byte/$1.cmi"
}

build_target_cmi() {
  echo "./_build/default/$1/.$1.objs/byte/$1.cmi"
}

# Git related helper scripts

# These variables are used by Git and set here so the name and email of the
# committer stay the same between different runs
export GIT_AUTHOR_NAME="Test Name"
export GIT_AUTHOR_EMAIL="test@example.com"
export GIT_COMMITTER_NAME="${GIT_AUTHOR_NAME}"
export GIT_COMMITTER_EMAIL="${GIT_AUTHOR_EMAIL}"
# Set various GIT variables to ensure git behaves as "default" as possible in the tests
export GIT_CONFIG_GLOBAL="/dev/null"
export GIT_CONFIG_SYSTEM="/dev/null"

export DUNE_RUNNING=0

# Called from cram tests with optional dune build arguments.
# shellcheck disable=SC2120
start_dune () {
    ( (dune build "$@" --passive-watch-mode > .#dune-output 2>&1) || (echo exit $? >> .#dune-output) ) &
    DUNE_PID=$!;
    export DUNE_RUNNING=1;
    wait_for_rpc_server
}

timeout="$(command -v timeout || echo gtimeout)"

with_timeout () {
    $timeout 2 "$@"
    exit_code=$?
    if [ "$exit_code" = 124 ]
    then
        echo Timed out
        cat .#dune-output
    else
        return "$exit_code"
    fi
}

with_timeout_quiet () {
    output=$(mktemp)
    $timeout 2 "$@" >"$output" 2>&1
    exit_code=$?
    if [ "$exit_code" = 124 ]
    then
        echo Timed out
        cat "$output"
        cat .#dune-output
    elif [ "$exit_code" != 0 ]
    then
        cat "$output"
    fi
    rm -f "$output"
    return "$exit_code"
}

shutdown_dune () {
    with_timeout dune shutdown
}

shutdown_dune_quiet () {
    with_timeout_quiet dune shutdown
}

wait_for_dune_exit () {
    # On Linux, we may run into a bash pid aliasing bug that causes wait to
    # reject the pid. Therefore we use tail to wait instead.
    if [ "$(uname -s)" = "Linux" ]
    then
        # wait for all child processes
        tail --pid="$DUNE_PID" -f /dev/null;
    else
        # wait for dune to exit
        wait "$DUNE_PID";
    fi
}

wait_for_pid_to_exit_with_timeout () {
    pid=$1
    iterations=$2
    while kill -0 "$pid" 2>/dev/null
    do
        if [ "$iterations" = 0 ]
        then
            return 124
        fi
        iterations=$((iterations - 1))
        sleep 0.01
    done
}

stop_dune () {
    shutdown_dune;
    wait_for_dune_exit;
    cat .#dune-output;
}

build () {
    with_timeout dune rpc build --wait "$@"
}

fs_memo_test () {
    local action="$1"
    local before between after

    echo "------------------------------------------"
    before=$(cat _build/default/result 2>/dev/null)
    # No initial targets: start the passive watch server only.
    # shellcheck disable=SC2119
    start_dune
    build . | grep -v Success
    between=$(cat _build/default/result)
    bash -c "$action"
    build . | grep -v Success
    stop_dune >> .#tmp
    after=$(cat _build/default/result)
    cat .#tmp
    echo "------------------------------------------"
    echo "result = '$before' -> '$between' -> '$after'"
    echo "------------------------------------------"
    dune trace cat | jq -c 'select(.name == "fs_update") | .args' | sort
    rm .#tmp
}

stop_dune_quiet () {
    shutdown_dune_quiet;
    wait_for_dune_exit_with_timeout;
}

build_quiet () {
    with_timeout_quiet dune rpc build --wait "$@"
}

wait_for_rpc_server () {
    with_timeout_quiet dune rpc ping --wait
}

summarize_rpc_trace () {
    dune trace cat | jq -r '
      select(.cat == "rpc")
      | if .name == "accept"
           and .args.stage == "stop"
           and .args.status == "close"
        then "accept stop close"
        elif .name == "accept"
             and .args.stage == "stop"
             and (.args | has("error"))
        then "accept stop error"
        elif .name == "startup-failure"
        then "startup failure"
        elif .name == "request" and .args.meth == "build"
        then "build \(.args.stage)"
        elif .name == "shutdown"
        then "shutdown \(.args.stage)"
        else empty
        end'
}

trace_rocq_flags () {
    local target="${1}"

    dune clean
    dune build "${target}"
    dune trace cat | jq_dune -c 'rocqFlags'
}

wait_for_dune_exit_with_timeout () {
    exit_code=0
    wait_for_pid_to_exit_with_timeout "$DUNE_PID" 200 || exit_code=$?
    if [ "$exit_code" = 124 ]
    then
        summarize_rpc_trace
    fi
    return "$exit_code"
}

wait_for_file () {
    until [ -e "$1" ]
    do
        sleep 0.01
    done
}

file_status() {
  [ -e "$1" ] && echo "$1 exists" || echo "$1 missing"
}

censor() {
  # Strip $PWD first so that sandbox hashes in the cram test's own path
  # aren't caught as digests. Each unique remaining digest gets a distinct
  # label ($DIGEST when unique, $DIGEST1/$DIGEST2/etc. when multiple).
  # shellcheck disable=SC2016
  dune_cmd subst "$PWD" '$PWD' \
    | dune_cmd subst-unique '[0-9a-f]{32}' '$DIGEST' \
    | dune_cmd subst-unique '\.cinaps\.[0-9a-f]{8}' '.cinaps.$CINAPS'
}

# Print PATH-like entries in $1 that aren't in $2, preserving their
# order in $1. Assumes ":" as the separator.
env_added() {
  tr ':' '\n' <<< "$1" | grep -vFxf <(tr ':' '\n' <<< "$2")
}
