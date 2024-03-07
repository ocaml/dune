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
    with_timeout dune shutdown;
    # On Linux, we may run into a bash pid aliasing bug that causes wait to
    # reject the pid. Therefore we use tail to wait instead.
    if [ "$(uname -s)" = "Linux" ]
    then
        # wait for all child processes
        tail --pid=$DUNE_PID -f /dev/null;
    else
        # wait for dune to exit
        wait $DUNE_PID;
    fi
    cat .#dune-output;
}

build () {
    with_timeout dune rpc build --wait "$@"
}
