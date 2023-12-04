Simple example of installing website files

  $ dune build @install

  $ cat _build/default/blog.install
  lib: [
    "_build/install/default/lib/blog/META"
    "_build/install/default/lib/blog/dune-package"
  ]
  share: [
    "_build/install/default/share/blog/content/about.html" {"content/about.html"}
    "_build/install/default/share/blog/content/posts/1.html" {"content/posts/1.html"}
    "_build/install/default/share/blog/content/posts/2.html" {"content/posts/2.html"}
    "_build/install/default/share/blog/content/posts/3.html" {"content/posts/3.html"}
    "_build/install/default/share/blog/style/bar.css" {"style/bar.css"}
    "_build/install/default/share/blog/style/foo.css" {"style/foo.css"}
  ]

  $ find _build/install | sort
  _build/install
  _build/install/default
  _build/install/default/lib
  _build/install/default/lib/blog
  _build/install/default/lib/blog/META
  _build/install/default/lib/blog/dune-package
  _build/install/default/share
  _build/install/default/share/blog
  _build/install/default/share/blog/content
  _build/install/default/share/blog/content/about.html
  _build/install/default/share/blog/content/posts
  _build/install/default/share/blog/content/posts/1.html
  _build/install/default/share/blog/content/posts/2.html
  _build/install/default/share/blog/content/posts/3.html
  _build/install/default/share/blog/style
  _build/install/default/share/blog/style/bar.css
  _build/install/default/share/blog/style/foo.css
