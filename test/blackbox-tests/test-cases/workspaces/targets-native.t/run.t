Workspaces also allow you to set "target" for cross compilation. This feature is
a bit hard to test since it requires mocking more than one context. But we can
see how we can set a "native" target. Which is the default.

  $ dune exec ./foo.exe
  message from targets-native test
