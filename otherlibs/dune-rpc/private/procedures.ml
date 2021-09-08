open Types
open Exported_types

let id x = x

module Public = struct
  module Ping = struct
    module V1 = struct
      let version = 1

      type req = unit

      type resp = unit

      type wire_req = unit

      type wire_resp = unit

      let req = Conv.unit

      let resp = Conv.unit

      let upgrade_req = id

      let upgrade_resp = id

      let downgrade_req = id

      let downgrade_resp = id
    end

    let decl =
      Decl.Request.make ~method_:"ping"
        ~generations:[ Decl.Request.make_gen (module V1) ]
  end

  module Diagnostics = struct
    module V1 = struct
      let version = 1

      type req = unit

      type resp = Diagnostic.t list

      type wire_req = unit

      type wire_resp = Diagnostic.t list

      let req = Conv.unit

      let resp = Conv.list Diagnostic.sexp

      let upgrade_req = id

      let upgrade_resp = id

      let downgrade_req = id

      let downgrade_resp = id
    end

    let decl =
      Decl.Request.make ~method_:"diagnostics"
        ~generations:[ Decl.Request.make_gen (module V1) ]
  end

  module Subscribe_ = struct
    module V1 = struct
      let version = 1

      type model = Subscribe.t

      type wire = Subscribe.t

      let sexp = Subscribe.sexp

      let upgrade = id

      let downgrade = id
    end

    let decl =
      Decl.Notification.make ~method_:"subscribe"
        ~generations:[ Decl.Notification.make_gen (module V1) ]
  end

  module Unsubscribe = struct
    module V1 = struct
      let version = 1

      type model = Subscribe.t

      type wire = Subscribe.t

      let sexp = Subscribe.sexp

      let upgrade = id

      let downgrade = id
    end

    let decl =
      Decl.Notification.make ~method_:"unsubscribe"
        ~generations:[ Decl.Notification.make_gen (module V1) ]
  end

  module Shutdown = struct
    module V1 = struct
      let version = 1

      type model = unit

      type wire = unit

      let sexp = Conv.unit

      let upgrade = id

      let downgrade = id
    end

    let decl =
      Decl.Notification.make ~method_:"shutdown"
        ~generations:[ Decl.Notification.make_gen (module V1) ]
  end

  module Format_dune_file = struct
    module V1 = struct
      let version = 1

      type req = Path.t * [ `Contents of string ]

      type resp = string

      type wire_req = Path.t * [ `Contents of string ]

      type wire_resp = string

      let req =
        let open Conv in
        let path = field "path" (required string) in
        let contents = field "contents" (required string) in
        let to_ (path, contents) = (path, `Contents contents) in
        let from (path, `Contents contents) = (path, contents) in
        iso (record (both path contents)) to_ from

      let resp = Conv.string

      let upgrade_req = id

      let downgrade_req = id

      let upgrade_resp = id

      let downgrade_resp = id
    end

    let decl =
      Decl.Request.make ~method_:"format-dune-file"
        ~generations:[ Decl.Request.make_gen (module V1) ]
  end

  module Promote = struct
    module V1 = struct
      let version = 1

      type req = Path.t

      type resp = unit

      type wire_req = Path.t

      type wire_resp = unit

      let req = Path.sexp

      let resp = Conv.unit

      let upgrade_req = id

      let downgrade_req = id

      let upgrade_resp = id

      let downgrade_resp = id
    end

    let decl =
      Decl.Request.make ~method_:"promote"
        ~generations:[ Decl.Request.make_gen (module V1) ]
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
    module V1 = struct
      let version = 1

      type req = string list

      type resp = Build_outcome.t

      type wire_req = string list

      type wire_resp = Build_outcome.t

      let req = Conv.list Conv.string

      let resp = Build_outcome.sexp

      let upgrade_req = id

      let downgrade_req = id

      let upgrade_resp = id

      let downgrade_resp = id
    end

    let decl =
      Decl.Request.make ~method_:"build"
        ~generations:[ Decl.Request.make_gen (module V1) ]
  end

  module Status = struct
    type req = unit

    type resp = Status.t

    module V1 = struct
      let version = 1

      type nonrec req = req

      type nonrec resp = resp

      type wire_req = req

      type wire_resp = resp

      let req = Conv.unit

      let resp = Status.sexp

      let upgrade_req = id

      let downgrade_req = id

      let upgrade_resp = id

      let downgrade_resp = id
    end

    let decl =
      Decl.Request.make ~method_:"status"
        ~generations:[ Decl.Request.make_gen (module V1) ]
  end

  let build = Build.decl

  let status = Status.decl
end

module Server_side = struct
  module Abort = struct
    module V1 = struct
      let version = 1

      type model = Message.t

      type wire = Message.t

      let sexp = Message.sexp

      let upgrade = id

      let downgrade = id
    end

    let decl =
      Decl.Notification.make ~method_:"notify/abort"
        ~generations:[ Decl.Notification.make_gen (module V1) ]
  end

  module Log = struct
    module V1 = struct
      let version = 1

      type model = Message.t

      type wire = Message.t

      let sexp = Message.sexp

      let upgrade = id

      let downgrade = id
    end

    let decl =
      Decl.Notification.make ~method_:"notify/log"
        ~generations:[ Decl.Notification.make_gen (module V1) ]
  end

  module Progress = struct
    module V1 = struct
      let version = 1

      type model = Progress.t

      type wire = Progress.t

      let sexp = Progress.sexp

      let upgrade = id

      let downgrade = id
    end

    let decl =
      Decl.Notification.make ~method_:"notify/progress"
        ~generations:[ Decl.Notification.make_gen (module V1) ]
  end

  module Diagnostic = struct
    module V1 = struct
      let version = 1

      type model = Diagnostic.Event.t list

      type wire = Diagnostic.Event.t list

      let sexp = Conv.list Diagnostic.Event.sexp

      let upgrade = id

      let downgrade = id
    end

    let decl =
      Decl.Notification.make ~method_:"notify/diagnostic"
        ~generations:[ Decl.Notification.make_gen (module V1) ]
  end

  let abort = Abort.decl

  let log = Log.decl

  let progress = Progress.decl

  let diagnostic = Diagnostic.decl
end
