let arg =
  Arg.(
    value
    & opt (some string) None
    & info
        [ "lang" ]
        ~docv:"VERSION"
        ~doc:
          "This argument has no effect and is deprecated. It exists solely for backwards \
           compatibility.")
;;
