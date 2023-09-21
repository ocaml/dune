- If `--ignore-promoted-rules` is set, do not change `(promote (until-clean))`
  rules and make affected rules fallback instead of removing them. This was
  previously done depending on `(lang dune)`. (#8721, @emillon)
