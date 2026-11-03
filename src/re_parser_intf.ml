open Base

module Definitions = struct
  module Applicative_delayed = struct
    module type S = sig
      type 'a t

      val return : 'a. 'a -> 'a t
      val apply : ('a -> 'b) t -> 'a t -> 'b t
      val both : 'a t -> 'b t -> ('a * 'b) t
      val map : 'a t -> f:('a -> 'b) -> 'b t
      val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
      val map3 : 'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd) -> 'd t
      val all : 'a t list -> 'a list t
      val all_unit : unit t list -> unit t
      val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
      val ( <* ) : 'a t -> unit t -> 'a t
      val ( *> ) : unit t -> 'a t -> 'a t
      val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

      module Applicative_infix : sig
        val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
        val ( <* ) : 'a t -> unit t -> 'a t
        val ( *> ) : unit t -> 'a t -> 'a t
        val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
      end
    end
  end

  module type T = sig
    type 'a t

    module Compiled : sig
      type 'a t
    end
  end

  module type Common = sig
    include T
    include Regex_parser_intf.S with type 'a t := 'a t with module Compiled := Compiled

    val string : string -> unit t
    val to_re : ?case_sensitive:bool -> _ t -> Re.t

    module Char : sig
      include Regex_parser_intf.Char with type 'a t := 'a t

      val one_of : char list -> char t
      val not_one_of : char list -> char t
    end
  end

  module type S_templated = sig
    include T
    include Applicative_delayed.S with type 'a t := 'a t

    val compile : ?case_sensitive:bool -> 'a t -> 'a Compiled.t
    val ignore_m : _ t -> unit t
    val capture : unit t -> string t
    val and_capture : 'a t -> ('a * string) t
    val or_ : 'a t list -> 'a t
    val optional : ?greedy:bool -> 'a t -> 'a option t
    val repeat : ?greedy:bool -> ?min:int -> ?max:int option -> unit t -> unit t
    val times : unit t -> int -> unit t
    val of_re : Re.t -> unit t
  end

  module type S = sig
    include T
    include Common with type 'a t := 'a t
    include S_templated with type 'a t := 'a t and module Compiled := Compiled
    include S_templated with type 'a t := 'a t and module Compiled := Compiled
  end

  module type S_portable = sig
    include T
    include Common with type 'a t := 'a t
    include S_templated with type 'a t := 'a t and module Compiled := Compiled
  end
end

module type Re_parser = sig
  open Definitions
  include T

  module Open_on_rhs_intf : sig
    module type S = S with type 'a t = 'a t
  end

  include
    Applicative.Let_syntax
    with type 'a t := 'a t
    with module Open_on_rhs_intf := Open_on_rhs_intf

  include Open_on_rhs_intf.S with type 'a t := 'a t

  module Portable : sig
    module Open_on_rhs_intf : sig
      module type S = S_portable with type 'a t = 'a t
    end

    (*_ Inlined [Applicative.Let_syntax] *)
    module Let_syntax : sig
      val return : 'a. 'a -> 'a t
      val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
      val ( <* ) : 'a t -> unit t -> 'a t
      val ( *> ) : unit t -> 'a t -> 'a t
      val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

      module Let_syntax : sig
        val return : 'a. 'a -> 'a t
        val map : 'a t -> f:('a -> 'b) -> 'b t
        val both : 'a t -> 'b t -> ('a * 'b) t

        module Open_on_rhs : Open_on_rhs_intf.S
      end
    end

    include Open_on_rhs_intf.S with type 'a t := 'a t with module Compiled := Compiled
  end
end
