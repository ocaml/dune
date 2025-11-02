open Types

module Request = struct
  type ('a, 'b) t = ('a, 'b) Decl.Request.witness

  let ping = Procedures.Public.ping.decl
  let diagnostics = Procedures.Public.diagnostics.decl
  let format = Procedures.Public.format.decl
  let format_dune_file = Procedures.Public.format_dune_file.decl
  let promote = Procedures.Public.promote.decl
  let promote_many = Procedures.Public.promote_many.decl
  let build_dir = Procedures.Public.build_dir.decl
  let runtest = Procedures.Public.runtest.decl
end

module Notification = struct
  type 'a t = 'a Decl.Notification.witness

  let shutdown = Procedures.Public.shutdown.decl
end

module Sub = struct
  type 'a t = 'a Sub.t

  let diagnostic = Sub.of_procedure Procedures.Poll.diagnostic
  let progress = Sub.of_procedure Procedures.Poll.progress
  let running_jobs = Sub.of_procedure Procedures.Poll.running_jobs
end
