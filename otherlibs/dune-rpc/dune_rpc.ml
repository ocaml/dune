module V1 = struct
  module Sexp = Stdune.Sexp
  open Dune_rpc_private
  module Id = Id
  module Response = Response
  module Initialize = Initialize.Request
  module Where = Where
  module Call = Call
  module Client = Client
  module Loc = Loc
  module Error = Error
  module Promotion = Promotion
  module Subscribe = Subscribe
  module Message = Message
  include Public

  module type S = sig
    type t

    type 'a fiber

    type chan

    module Handler : sig
      type t

      val create :
           ?log:(Message.t -> unit fiber)
        -> ?errors:(Error.t list -> unit fiber)
        -> ?promotions:(Promotion.t list -> unit fiber)
        -> ?abort:(Message.t -> unit fiber)
        -> unit
        -> t
    end

    val request :
         ?id:Id.t
      -> t
      -> ('a, 'b) Request.t
      -> 'a
      -> ('b, Response.Error.t) result fiber

    val notification : t -> 'a Notification.t -> 'a -> unit fiber

    val connect :
         ?handler:Handler.t
      -> chan
      -> Initialize.t
      -> f:(t -> 'a fiber)
      -> 'a fiber
  end
end
