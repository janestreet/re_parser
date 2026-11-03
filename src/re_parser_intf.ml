open Base

module Definitions = struct
  module Applicative_delayed = struct
    module type%template
      [@mode
        p_name = (nonportable, portable)
        , (p, c) = ((nonportable, uncontended), (portable, contended))]
      S = sig
      type 'a t

      [@@@mode.default p_name]

      val return : ('a : value mod c). 'a @ p -> 'a t @ p
      val apply : ('a -> 'b) t @ p -> 'a t @ p -> 'b t @ p
      val both : 'a t @ p -> 'b t @ p -> ('a * 'b) t @ p
      val map : 'a t @ p -> f:('a -> 'b) @ p -> 'b t @ p
      val map2 : 'a t @ p -> 'b t @ p -> f:('a -> 'b -> 'c) @ p -> 'c t @ p

      val map3
        :  'a t @ p
        -> 'b t @ p
        -> 'c t @ p
        -> f:('a -> 'b -> 'c -> 'd) @ p
        -> 'd t @ p

      val all : 'a t list @ p -> 'a list t @ p
      val all_unit : unit t list @ p -> unit t @ p
      val ( <*> ) : ('a -> 'b) t @ p -> 'a t @ p -> 'b t @ p
      val ( <* ) : 'a t @ p -> unit t @ p -> 'a t @ p
      val ( *> ) : unit t @ p -> 'a t @ p -> 'a t @ p
      val ( >>| ) : 'a t @ p -> ('a -> 'b) @ p -> 'b t @ p

      module Applicative_infix : sig
        val ( <*> ) : ('a -> 'b) t @ p -> 'a t @ p -> 'b t @ p
        val ( <* ) : 'a t @ p -> unit t @ p -> 'a t @ p
        val ( *> ) : unit t @ p -> 'a t @ p -> 'a t @ p
        val ( >>| ) : 'a t @ p -> ('a -> 'b) @ p -> 'b t @ p
      end
    end
  end

  module type T = sig
    type 'a t : value mod contended

    module Compiled : sig
      type 'a t : value mod contended
    end
  end

  module type Common = sig @@ portable
    include T
    include Regex_parser_intf.S with type 'a t := 'a t with module Compiled := Compiled

    val string : string -> unit t @ portable
    val to_re : ?case_sensitive:bool -> _ t -> Re.t

    module Char : sig
      include Regex_parser_intf.Char with type 'a t := 'a t

      val one_of : char list -> char t @ portable
      val not_one_of : char list -> char t @ portable
    end
  end

  module type%template
    [@mode
      p_name = (nonportable, portable)
      , (p, c) = ((nonportable, uncontended), (portable, contended))]
    S_templated = sig
    @@ portable
    include T
    include Applicative_delayed.S [@mode p_name p] with type 'a t := 'a t

    [@@@mode.default p_name]

    val compile : ?case_sensitive:bool -> 'a t @ p -> 'a Compiled.t @ p
    val ignore_m : _ t @ p -> unit t @ p
    val capture : unit t @ p -> string t @ p
    val and_capture : 'a t @ p -> ('a * string) t @ p
    val or_ : 'a t list @ p -> 'a t @ p
    val optional : ?greedy:bool -> 'a t @ p -> 'a option t @ p
    val repeat : ?greedy:bool -> ?min:int -> ?max:int option -> unit t @ p -> unit t @ p
    val times : unit t @ p -> int -> unit t @ p
    val of_re : Re.t -> unit t @ p
  end

  module type S = sig
    include T
    include Common with type 'a t := 'a t

    include%template
      S_templated
      [@mode nonportable nonportable]
      with type 'a t := 'a t
       and module Compiled := Compiled

    include%template
      S_templated
      [@mode portable portable]
      with type 'a t := 'a t
       and module Compiled := Compiled
  end

  module type S_portable = sig
    include T
    include Common with type 'a t := 'a t

    include%template
      S_templated
      [@mode nonportable portable]
      with type 'a t := 'a t
       and module Compiled := Compiled
  end
end

module type Re_parser = sig @@ portable
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
      val return : ('a : value mod contended). 'a @ portable -> 'a t @ portable
      val ( <*> ) : ('a -> 'b) t @ portable -> 'a t @ portable -> 'b t @ portable
      val ( <* ) : 'a t @ portable -> unit t @ portable -> 'a t @ portable
      val ( *> ) : unit t @ portable -> 'a t @ portable -> 'a t @ portable
      val ( >>| ) : 'a t @ portable -> ('a -> 'b) @ portable -> 'b t @ portable

      module Let_syntax : sig
        val return : ('a : value mod contended). 'a @ portable -> 'a t @ portable
        val map : 'a t @ portable -> f:('a -> 'b) @ portable -> 'b t @ portable
        val both : 'a t @ portable -> 'b t @ portable -> ('a * 'b) t @ portable

        module Open_on_rhs : Open_on_rhs_intf.S
      end
    end

    include Open_on_rhs_intf.S with type 'a t := 'a t with module Compiled := Compiled
  end
end
