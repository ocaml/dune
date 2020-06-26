This example creates a configuration file using a fallback rule.
See README.org for details

By default, config is set:

  $ dune exec ./src/plop.exe
  Built with support for blah: true
  Path to blah is:             "/path/to/blah"

Print generated config:

  $ cat _build/default/config.full
  
  let with_blah = true
  let blah_path = "/path/to/blah"

Now, we disable it, and re-run:

  $ echo "let enable_blah = No" > config
  $ dune exec ./src/plop.exe
  Built with support for blah: false
  Path to blah is:             "<no support for blah>"
  $ cat _build/default/config.full
  
  let with_blah = false
  let blah_path = "<no support for blah>"
