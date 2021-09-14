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

  module Subscribe_ = struct
    let v1 = Decl.Notification.make_current_gen ~conv:Subscribe.sexp ~version:1

    let decl = Decl.Notification.make ~method_:"subscribe" ~generations:[ v1 ]
  end

  module Unsubscribe = struct
    let v1 = Decl.Notification.make_current_gen ~conv:Subscribe.sexp ~version:1

    let decl = Decl.Notification.make ~method_:"unsubscribe" ~generations:[ v1 ]
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

  let ping = Ping.decl

  let diagnostics = Diagnostics.decl

  let subscribe = Subscribe_.decl

  let unsubscribe = Unsubscribe.decl

  let shutdown = Shutdown.decl

  let format_dune_file = Format_dune_file.decl

  let promote = Promote.decl
end

module Internal = struct
  module Build = struct
    let v1 =
      Decl.Request.make_current_gen ~req:(Conv.list Conv.string)
        ~resp:Build_outcome.sexp ~version:1

    let decl = Decl.Request.make ~method_:"build" ~generations:[ v1 ]
  end

  module Status = struct
    let v1 =
      Decl.Request.make_current_gen ~req:Conv.unit ~resp:Status.sexp ~version:1

    let decl = Decl.Request.make ~method_:"status" ~generations:[ v1 ]
  end

  let build = Build.decl

  let status = Status.decl
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

  module Progress = struct
    let v1 = Decl.Notification.make_current_gen ~conv:Progress.sexp ~version:1

    let decl =
      Decl.Notification.make ~method_:"notify/progress" ~generations:[ v1 ]
  end

  module Diagnostic = struct
    let v1 =
      Decl.Notification.make_current_gen
        ~conv:(Conv.list Diagnostic.Event.sexp)
        ~version:1

    let decl =
      Decl.Notification.make ~method_:"notify/diagnostic" ~generations:[ v1 ]
  end

  let abort = Abort.decl

  let log = Log.decl

  let progress = Progress.decl

  let diagnostic = Diagnostic.decl
end
