# Here we configure a local git user for our tests
git_init() {
  command git init --quiet $@
  git config user.name "Test Name"
  git config user.email "test@example.com"
}

# Define a function to override 'git' so that it is used correctly in tests.
git() {
  if [[ "$1" == "init" ]]; then
    echo "ERROR: 'git init' is not recommended for this test setup."
    echo "Please use 'git_init' from 'helpers.sh' instead."
    return 1
  else
    command git "$@"
  fi
}
