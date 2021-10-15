We can use our local packages, in our documentation thanks to dune's mdx stanza:

```ocaml
# #require "pkg.public-lib";;
# Public_lib.x;;
- : int = 1
```

```ocaml
# #require "pkg2.public-lib2";;
# Public_lib2.x;;
- : int = 2
```
