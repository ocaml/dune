open Import
open Types
open Exported_types

module Public = struct
  module Ping = struct
    let v1 =
      Decl.Request.make_current_gen ~req:Conv.unit ~resp:Conv.unit ~version:1

    let decl = Decl.Request.make ~method_:"ping" ~generations:[ v1 ]
  end

  module Diagnostics = struct
    let v1 =
      Decl.Request.make_current_gen ~req:Conv.unit
        ~resp:(Conv.list Diagnostic.sexp)
        ~version:1

    let decl = Decl.Request.make ~method_:"diagnostics" ~generations:[ v1 ]
  end

  module Shutdown = struct
    let v1 = Decl.Notification.make_current_gen ~conv:Conv.unit ~version:1

    let decl = Decl.Notification.make ~method_:"shutdown" ~generations:[ v1 ]
  end

  module Format_dune_file = struct
    module V1 = struct
      let req =
        let open Conv in
        let path = field "path" (required string) in
        let contents = field "contents" (required string) in
        let to_ (path, contents) = (path, `Contents contents) in
        let from (path, `Contents contents) = (path, contents) in
        iso (record (both path contents)) to_ from
    end

    let v1 =
      Decl.Request.make_current_gen ~req:V1.req ~resp:Conv.string ~version:1

    let decl = Decl.Request.make ~method_:"format-dune-file" ~generations:[ v1 ]
  end

  module Promote = struct
    let v1 =
      Decl.Request.make_current_gen ~req:Path.sexp ~resp:Conv.unit ~version:1

    let decl = Decl.Request.make ~method_:"promote" ~generations:[ v1 ]
  end

  module Build_dir = struct
    let v1 =
      Decl.Request.make_current_gen ~req:Conv.unit ~resp:Path.sexp ~version:1

    let decl = Decl.Request.make ~method_:"build_dir" ~generations:[ v1 ]
  end

  let ping = Ping.decl

  let diagnostics = Diagnostics.decl

  let shutdown = Shutdown.decl

  let format_dune_file = Format_dune_file.decl

  let promote = Promote.decl

  let build_dir = Build_dir.decl
end

module Server_side = struct
  module Abort = struct
    let v1 = Decl.Notification.make_current_gen ~conv:Message.sexp ~version:1

    let decl =
      Decl.Notification.make ~method_:"notify/abort" ~generations:[ v1 ]
  end

  module Log = struct
    let v1 = Decl.Notification.make_current_gen ~conv:Message.sexp ~version:1

    let decl = Decl.Notification.make ~method_:"notify/log" ~generations:[ v1 ]
  end

  let abort = Abort.decl

  let log = Log.decl
end

module Poll = struct
  let cancel_gen = Decl.Notification.make_current_gen ~conv:Id.sexp ~version:1

  module Name = struct
    include String

    let make s = s
  end

  type 'a t =
    { poll : (Id.t, 'a option) Decl.request
    ; cancel : Id.t Decl.notification
    ; name : Name.t
    }

  let make name generations =
    let poll = Decl.Request.make ~method_:("poll/" ^ name) ~generations in
    let cancel =
      Decl.Notification.make ~method_:("cancel-poll/" ^ name)
        ~generations:[ cancel_gen ]
    in
    { poll; cancel; name }

  let poll t = t.poll

  let cancel t = t.cancel

  let name t = t.name

  module Progress = struct
    let name = "progress"

    let v1 =
      Decl.Request.make_current_gen ~req:Id.sexp
        ~resp:(Conv.option Progress.sexp)
        ~version:1
  end

  module Diagnostic = struct
    let name = "diagnostic"

    let v1 =
      Decl.Request.make_current_gen ~req:Id.sexp
        ~resp:(Conv.option (Conv.list Diagnostic.Event.sexp))
        ~version:1
  end

  let progress =
    let open Progress in
    make name [ v1 ]

  let diagnostic =
    let open Diagnostic in
    make name [ v1 ]
end
