Testing the changelog adding script

  $ cat > user_response << EOF
  > Changelog description that is long enough to be wrapped to the next line
  > 1234
  > SomeGitHubUser
  > 123 456 789
  > EOF

  $ ./add.exe < user_response
  What is the change about?
  PR number? (Leave blank if you don't know)
  What is your username? (This will be saved for later)
  Which issue numbers does this change fix?
  Changelog entry in: doc/changes/1234.md

  $ ls doc/changes/
  1234.md
  username
  $ cat doc/changes/*.md
  - Changelog description that is long enough to be wrapped to the next line
    (#1234, fixes #123, #456 and #789, @SomeGitHubUser)
  

The username gets recorded in doc/changes/username

  $ cat doc/changes/username
  SomeGitHubUser

Omitted the PR number and issues. Also reusing the saved username.

  $ cat > user_response << EOF
  > Changelog description that is long enough to be wrapped to the next line. \
  > This one takes the cake for being especially long and wrappable.
  > 
  > 
  > EOF

  $ ./add.exe < user_response
  What is the change about?
  PR number? (Leave blank if you don't know)
  Using username from doc/changes/username.
  Which issue numbers does this change fix?
  Changelog entry in: doc/changes/????.md

  $ ls doc/changes/
  1234.md
  ????.md
  username
  $ cat doc/changes/*.md
  - Changelog description that is long enough to be wrapped to the next line
    (#1234, fixes #123, #456 and #789, @SomeGitHubUser)
  
  - Changelog description that is long enough to be wrapped to the next line.
    This one takes the cake for being especially long and wrappable. (#????,
    @SomeGitHubUser)
  
To use another username, simply delete the doc/changes/username file.

  $ rm doc/changes/username

This time we also omit the PR number and give the same username to cause a
conflict with the entry.

  $ cat > user_response << EOF
  > Another really long changelog description with some information that makes \
  > it long enough to wrap to the next line.
  > 
  > SomeGitHubUser
  > 1234 7809
  > EOF

  $ ./add.exe < user_response
  What is the change about?
  PR number? (Leave blank if you don't know)
  What is your username? (This will be saved for later)
  Which issue numbers does this change fix?
  Error: File doc/changes/????.md already exists.
  [1]

In this case, the ????_SomeGithubUser file should have already been renamed
since the PR should be known by now. (? should be quoted in the shell) 
  $ (cd doc/changes/ && mv "????.md" 7890.md)
 
  $ ./add.exe < user_response
  What is the change about?
  PR number? (Leave blank if you don't know)
  Using username from doc/changes/username.
  Which issue numbers does this change fix?
  Changelog entry in: doc/changes/????.md

