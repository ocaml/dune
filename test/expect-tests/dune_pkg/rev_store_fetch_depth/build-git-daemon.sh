#!/bin/sh

# Script to construct a git-daemon binary if it exists. Not all git
# installations contain git-daemon, but it's also not in $PATH so this script
# attempts to create a wrapper

# if git-daemon exists, it lives here
daemon="$(git --exec-path)/git-daemon"

if [ -x "$daemon" ]; then
  cat > git-daemon <<EOF
#!/bin/sh
exec "$daemon" "$@"
EOF
  chmod +x git-daemon
else
  >&2 echo "git-daemon missing; make sure your git installation contains git-daemon"
  exit 1
fi
