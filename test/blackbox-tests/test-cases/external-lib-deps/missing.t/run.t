Expected: Only required packages are hinted, and no wrapping occurs in the install command.

  $ dune external-lib-deps --missing @all
  Error: The following libraries are missing in the default context:
  - a________
  - b________
  - c________
  - d________
  - e________
  - f________
  - h________
  - i________
  - j________
  - optional (optional)
  Hint: try:
    opam install a________ b________ c________ d________ e________ f________ h________ i________ j________
  [1]
