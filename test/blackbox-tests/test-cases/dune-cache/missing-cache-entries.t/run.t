Check that Dune cache can cope with missing file/metadata entries.

  $ export DUNE_CACHE_ROOT=$PWD/.cache

  $ function find_unique_entry_containing {
  >   (all_entries_x=$(cd "$DUNE_CACHE_ROOT"; grep -rsl . -e "$1"; echo -n x)
  >   print_all_entries () {
  >     printf "%s" "$all_entries_x" | head -c -1
  >   }
  >   count="$(print_all_entries | wc -l)"
  >   if [ "$count" != 1 ];
  >   then
  >     printf "found %d entries:\n" "$count"
  >     print_all_entries >&2;
  >     false
  >   else
  >     print_all_entries
  >   fi)
  > }

  $ cat > config <<EOF
  > (lang dune 2.1)
  > (sandboxing_preference none)
  > (cache enabled)
  > (cache-duplication copy)
  > (cache-transport direct)
  > EOF
  $ cat > dune-project <<EOF
  > (lang dune 2.1)
  > EOF
  $ cat > dune <<EOF
  > (rule
  >   (deps source)
  >   (targets target)
  >   (action (bash "cat source source > target")))
  > (rule
  >   (deps source)
  >   (targets twin-a twin-b)
  >   (action (bash "echo twin-a-contents >> twin-a; echo twin-b-contents >> twin-b")))
  > EOF
  $ cat > source <<EOF
  > \_o< COIN
  > EOF

  $ dune build --config-file=config target twin-a twin-b
  $ cat _build/default/target
  \_o< COIN
  \_o< COIN
  $ cat _build/default/twin-{a,b}
  twin-a-contents
  twin-b-contents

Delete the [files/v4] storage and test that Dune can cope with this.

  $ rm -rf _build "$DUNE_CACHE_ROOT"/files/v4/
  $ dune build --config-file=config target
  $ cat _build/default/target
  \_o< COIN
  \_o< COIN

Same but for the [meta/v5] storage.

  $ rm -rf _build "$DUNE_CACHE_ROOT"/meta/v5/
  $ dune build --config-file=config target
  $ cat _build/default/target
  \_o< COIN
  \_o< COIN

Now nuke the whole cache directory.

  $ rm -rf "$DUNE_CACHE_ROOT"
  $ rm -rf _build
  $ dune build --config-file=config target twin-a twin-b
  $ cat _build/default/target
  \_o< COIN
  \_o< COIN

Selectively delete just one of the set of targets.

  $ twin_a_entry=$(find_unique_entry_containing "twin-b-contents")
  $ rm "$DUNE_CACHE_ROOT"/"$twin_a_entry"
  $ rm -r _build

  $ dune build --config-file=config twin-a
  File "dune", line 5, characters 0-131:
  5 | (rule
  6 |   (deps source)
  7 |   (targets twin-a twin-b)
  8 |   (action (bash "echo twin-a-contents >> twin-a; echo twin-b-contents >> twin-b")))
          bash twin-a,twin-b (exit 1)
  (cd _build/default && /usr/bin/bash -e -u -o pipefail -c 'echo twin-a-contents >> twin-a; echo twin-b-contents >> twin-b')
  /usr/bin/bash: twin-a: Permission denied
  [1]
  $ cat _build/default/twin-a
  cat: _build/default/twin-a: No such file or directory
  [1]
  $ cat _build/default/twin-b
  cat: _build/default/twin-b: No such file or directory
  [1]
