open Import

let client ?handler connection init ~f =
  Client.client
    ?handler
    ~private_menu:[ Request Decl.build; Request Decl.status ]
    connection
    init
    ~f
;;
