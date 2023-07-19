let load file =
  if Dune_site_private.debug then
    Printf.printf "DUNE_SITE_DEBUG[dynlink]: loading %s\n" file;
  Dynlink.loadfile file
