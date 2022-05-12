open Import

let latest_lang_version =
  ( (let+ () = Term.const () in
     print_endline
       (Dune_lang.Syntax.greatest_supported_version Stanza.syntax
       |> Dune_lang.Syntax.Version.to_string))
  , Term.info "latest-lang-version" )

let group =
  ( Term.Group.Group
      [ in_group Internal_dump.command; in_group latest_lang_version ]
  , Term.info "internal" )
