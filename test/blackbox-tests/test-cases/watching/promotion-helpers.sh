write_copy_promotion_project () {
    echo '(lang dune 3.0)' > dune-project
    cat > dune <<'EOF'
(rule
 (mode promote)
 (deps original)
 (target promoted)
 (action (copy %{deps} %{target})))
(rule
 (deps promoted)
 (target result)
 (action (system "cat promoted promoted > result")))
EOF
}

write_promote_and_source_copy_project () {
    local runs="$1"

    echo '(lang dune 3.23)' > dune-project
    mkdir sub
    cat > dune <<EOF
(rule
 (deps data)
 (target result)
 (action
  (bash "cat data > result; printf 'copy:%s\n' \"\$(cat data)\" >> $runs")))
(alias
 (name idle))
EOF
    cat > sub/dune <<EOF
(rule
 (mode (promote (into ..)))
 (target data)
 (action (bash "printf new > data; printf 'promote\n' >> $runs")))
EOF
}

write_promoted_result_project () {
    local promote_action="$1"
    local result_action="$2"

    echo '(lang dune 3.0)' > dune-project
    cat > dune <<EOF
(rule
 (mode promote)
 (deps original (sandbox none))
 (target promoted)
 (action
  (bash "$promote_action")))
(rule
 (deps promoted (sandbox none))
 (target result)
 (action
  (bash "$result_action")))
(alias
 (name idle))
EOF
}
