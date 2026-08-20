open Import

let client ?handler connection init ~f =
  Client.client
    ?handler
    ~private_menu:
      [ Request Decl.build
      ; Request Decl.status
      ; Request Decl.pkg_enabled
      ; Request Decl.simulate_file_watcher_queue_overflow
      ]
    connection
    init
    ~f
;;
