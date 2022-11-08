DUNE_RUNNING=0

start_dune () {
    ((dune build "$@" --passive-watch-mode > .#dune-output 2>&1) || (echo exit $? >> .#dune-output)) &
    DUNE_PID=$!;
    DUNE_RUNNING=1;
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

stop_dune () {
    with_timeout dune shutdown
    cat .#dune-output
}

build () {
    with_timeout dune rpc build --wait "$@"
}
