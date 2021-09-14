We can use our local packages, in our documentation thanks to dune's mdx stanza:

```ocaml
# #require "pkg.public-lib";;
# Public_lib.x;;
- : int = 0
```

We can also use our local public executables:

```sh
$ public-bin
Hey!
```
