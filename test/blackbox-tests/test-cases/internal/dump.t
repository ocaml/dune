Test the "dune internal dump" command.

  $ echo '(lang dune 3.0)' > dune-project
  $ touch x
  $ dune build x
  $ dune internal dump _build/.digest-db 2>&1 | sed 's/timestamp = .*/timestamp = -/'
  { checked_key = 0
  ; max_timestamp = -
  ; table =
      map
        { In_source_tree "x" :
            { digest = digest "\18461\1934\169d\158\195\131\208\235\1565h\003"
            ; timestamp = -
            ; size = 0
            ; permissions = 436
            ; stats_checked = 0
            }
        }
  }
