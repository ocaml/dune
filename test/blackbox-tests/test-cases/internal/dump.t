Test the "dune internal dump" command.

  $ echo '(lang dune 3.0)' > dune-project
  $ touch x
  $ dune build x
  $ dune internal dump _build/.digest-db 2>&1 | \
  > sed 's/timestamp = .*/timestamp = -/;s/permissions = .*/permissions = -/'
  { checked_key = 0
  ; max_timestamp = -
  ; table =
      map
        { In_source_tree "x" :
            { digest = digest "b83631c134a9649ec383d0eb9c356803"
            ; timestamp = -
            ; size = 0
            ; permissions = -
            ; stats_checked = 0
            }
        }
  }
