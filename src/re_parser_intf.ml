open Base

module type S = sig
  type 'a t

  include Regex_parser_intf.S with type 'a t := 'a t

  val to_re : ?case_sensitive:bool -> _ t -> Re.t
  val of_re : Re.t -> unit t
end

module type Re_parser = sig
  type 'a t

  module Open_on_rhs_intf : sig
    module type S = S with type 'a t = 'a t
  end

  include
    Applicative.Let_syntax
    with type 'a t := 'a t
    with module Open_on_rhs_intf := Open_on_rhs_intf

  include Open_on_rhs_intf.S with type 'a t := 'a t
end
