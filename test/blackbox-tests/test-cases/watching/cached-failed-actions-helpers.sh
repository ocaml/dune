setup_failing_action_script () {
    cat > fail.sh <<'EOF'
counter="$1"
count=0
if [ -e "$counter" ]; then count=$(cat "$counter"); fi
count=$((count + 1))
echo "$count" > "$counter"
echo failed action >&2
exit 1
EOF
}

print_build_finish_process_counts () {
    dune trace cat | jq -s '
[ .[]
| select(.cat == "build" and .name == "build-finish")
| { outcome: .args.outcome, process_count: .args.process_times.count } ]'
}
