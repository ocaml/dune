- Reject `(package ...)` inside a named dependency binding
  (`(deps (:name (package foo)))`). Previously this was silently
  accepted but `%{name}` would resolve to an empty path list. (@Alizter)
