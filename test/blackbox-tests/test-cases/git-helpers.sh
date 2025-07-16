# Git related helper scripts

# This function overrides 'git' so that it is used correctly in tests. It has
# the following properties:
#
# - When we initialise a repository, we always want to set a git user manually.
# 
git() {
  if [ "$1" = "init" ]; then
    command git "$@"
    command git config user.name "Test Name"
    command git config user.email "test@example.com"
  else
    command git "$@"
  fi
}
