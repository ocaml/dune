env_added() {
  comm -23 <(tr ':' '\n' <<< "$1" | sort -u) <(tr ':' '\n' <<< "$2" | sort -u) \
    | censor_install_layout
}
