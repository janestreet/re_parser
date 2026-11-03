open! Base
open Re_parser_intf.Definitions

module%template Maybe_portable = struct
  let[@mode nonportable] wrap = Fn.id
  let[@mode portable] wrap = Modes.Portable.wrap
  let[@mode nonportable] unwrap = Fn.id
  let[@mode portable] unwrap = Modes.Portable.unwrap
  let[@mode nonportable] wrap_list = Fn.id
  let[@mode portable] wrap_list = Modes.Portable.wrap_list
end

module Body : S = struct
  type 'a t =
    { regex : Re.t
    ; num_captures : int
    ; extract_value_from_captured_groups : captures:Re.Group.t -> offset:int -> 'a option
    }

  [%%template
  let return (type a : value mod c) (x : a @ p) : a t @ p =
    { regex = Re.epsilon
    ; num_captures = 0
    ; extract_value_from_captured_groups = (fun ~captures:_ ~offset:_ -> Some x)
    }
  [@@mode (p, c) = ((nonportable, uncontended), (portable, contended))]
  ;;

  [@@@mode.default p = (nonportable, portable)]

  let map (t : 'a t @ p) ~(f : ('a -> 'b) @ p) : 'b t @ p =
    { t with
      extract_value_from_captured_groups =
        (fun ~captures ~offset ->
          Option.map (t.extract_value_from_captured_groups ~captures ~offset) ~f)
    }
  ;;

  let apply (tf : ('a -> 'b) t @ p) (tx : 'a t @ p) : 'b t @ p =
    let extract_value_from_captured_groups ~captures ~offset =
      let f = tf.extract_value_from_captured_groups ~captures ~offset in
      let x =
        tx.extract_value_from_captured_groups ~captures ~offset:(offset + tf.num_captures)
      in
      Option.map2 f x ~f:(fun f x -> f x)
    in
    { regex = Re.seq [ tf.regex; tx.regex ]
    ; num_captures = tf.num_captures + tx.num_captures
    ; extract_value_from_captured_groups
    }
  ;;

  (* Inlined from [Applicative.Make] *)

  let map2 ta tb ~f = (apply [@mode p]) ((map [@mode p]) ta ~f) tb

  let map3 ta tb tc ~f =
    (apply [@mode p]) ((apply [@mode p]) ((map [@mode p]) ta ~f) tb) tc
  ;;

  let both ta tb = (map2 [@mode p]) ta tb ~f:(fun a b -> a, b)
  let ( <*> ) = (apply [@mode p])
  let ( >>| ) t f = (map [@mode p]) t ~f

  let ( *> ) u v =
    (apply [@mode p]) ((apply [@mode p]) ((return [@mode p]) (fun () y -> y)) u) v
  ;;

  let ( <* ) u v =
    (apply [@mode p]) ((apply [@mode p]) ((return [@mode p]) (fun x () -> x)) u) v
  ;;

  let all (ts : 'a t list @ p) : 'a list t @ p =
    let ts = (Maybe_portable.wrap_list [@mode p]) ts in
    List.fold_right
      ts
      ~init:
        ((Maybe_portable.wrap [@mode p])
           { regex = Re.epsilon
           ; num_captures = 0
           ; extract_value_from_captured_groups = (fun ~captures:_ ~offset:_ -> Some [])
           })
      ~f:(fun t acc ->
        (Maybe_portable.wrap [@mode p])
          ((map2 [@mode p])
             ((Maybe_portable.unwrap [@mode p]) t)
             ((Maybe_portable.unwrap [@mode p]) acc)
             ~f:(fun x xs -> x :: xs)))
    |> (Maybe_portable.unwrap [@mode p])
  ;;

  let all_unit (ts : unit t list @ p) : unit t @ p =
    let ts = (Maybe_portable.wrap_list [@mode p]) ts in
    List.fold
      ts
      ~init:((Maybe_portable.wrap [@mode p]) ((return [@mode p]) ()))
      ~f:(fun acc t ->
        (Maybe_portable.wrap [@mode p])
          ((( *> ) [@mode p])
             ((Maybe_portable.unwrap [@mode p]) acc)
             ((Maybe_portable.unwrap [@mode p]) t)))
    |> (Maybe_portable.unwrap [@mode p])
  ;;

  module Applicative_infix = struct
    let ( <*> ) = (( <*> ) [@mode p])
    let ( *> ) = (( *> ) [@mode p])
    let ( <* ) = (( <* ) [@mode p])
    let ( >>| ) = (( >>| ) [@mode p])
  end]

  let extract_nothing ~captures:_ ~offset:_ = Some ()
  let get_first_string_of_group ~captures ~offset = Re.Group.get_opt captures offset

  module Char = struct
    module Re2_compatiblity = struct
      let num_letters_in_alphabet = 26

      let upper =
        List.init num_letters_in_alphabet ~f:(fun i -> i + 65 |> Char.of_int_exn)
        |> String.of_char_list
      ;;

      let lower =
        List.init num_letters_in_alphabet ~f:(fun i -> i + 97 |> Char.of_int_exn)
        |> String.of_char_list
      ;;

      let digit =
        List.init 10 ~f:(fun i -> i + 48 |> Char.of_int_exn) |> String.of_char_list
      ;;

      let alpha = upper ^ lower
      let alnum = alpha ^ digit
    end

    let get_first_char_of_group ~captures ~offset =
      Re.Group.get_opt captures offset |> Option.map ~f:Char.of_string
    ;;

    let any : 'a t =
      { regex = Re.group Re.any
      ; num_captures = 1
      ; extract_value_from_captured_groups = get_first_char_of_group
      }
    ;;

    let digit : 'a t =
      { regex = Re.group Re.digit
      ; num_captures = 1
      ; extract_value_from_captured_groups = get_first_char_of_group
      }
    ;;

    let space : 'a t =
      { regex = Re.group Re.space
      ; num_captures = 1
      ; extract_value_from_captured_groups = get_first_char_of_group
      }
    ;;

    let upper : 'a t =
      { regex = Re.group (Re.set Re2_compatiblity.upper)
      ; num_captures = 1
      ; extract_value_from_captured_groups = get_first_char_of_group
      }
    ;;

    let lower : 'a t =
      { regex = Re.group (Re.set Re2_compatiblity.lower)
      ; num_captures = 1
      ; extract_value_from_captured_groups = get_first_char_of_group
      }
    ;;

    let alpha : char t =
      { regex = Re.group (Re.set Re2_compatiblity.alpha)
      ; num_captures = 1
      ; extract_value_from_captured_groups = get_first_char_of_group
      }
    ;;

    let alnum : char t =
      { regex = Re.group (Re.set Re2_compatiblity.alnum)
      ; num_captures = 1
      ; extract_value_from_captured_groups = get_first_char_of_group
      }
    ;;

    let one_of (chars : char list) : char t =
      { regex = Re.group (Re.set (String.of_char_list chars))
      ; num_captures = 1
      ; extract_value_from_captured_groups = get_first_char_of_group
      }
    ;;

    let not_one_of (chars : char list) : char t =
      { regex = Re.group (Re.compl [ Re.set (String.of_char_list chars) ])
      ; num_captures = 1
      ; extract_value_from_captured_groups = get_first_char_of_group
      }
    ;;
  end

  let string (s : string) : unit t =
    { regex = Re.str s
    ; num_captures = 0
    ; extract_value_from_captured_groups = extract_nothing
    }
  ;;

  let any_string : string t =
    { regex = Re.group (Re.rep Re.any)
    ; num_captures = 1
    ; extract_value_from_captured_groups = get_first_string_of_group
    }
  ;;

  let start_of_input : unit t =
    { regex = Re.bos
    ; num_captures = 0
    ; extract_value_from_captured_groups = extract_nothing
    }
  ;;

  let end_of_input : unit t =
    { regex = Re.eos
    ; num_captures = 0
    ; extract_value_from_captured_groups = extract_nothing
    }
  ;;

  let%template ignore_m (t : 'a t @ p) : unit t @ p =
    { regex = Re.no_group t.regex
    ; num_captures = 0
    ; extract_value_from_captured_groups = extract_nothing
    }
  [@@mode p = (nonportable, portable)]
  ;;

  let%template and_capture (t : 'a t @ p) : ('a * string) t @ p =
    { regex = Re.group t.regex
    ; num_captures = t.num_captures + 1
    ; extract_value_from_captured_groups =
        (fun ~captures ~offset ->
          let full_capture : string option =
            get_first_string_of_group ~captures ~offset
          in
          let previous_capture : 'a option =
            t.extract_value_from_captured_groups ~captures ~offset:(offset + 1)
          in
          Option.both previous_capture full_capture)
    }
  [@@mode p = (nonportable, portable)]
  ;;

  let%template capture t = (map [@mode p]) ((and_capture [@mode p]) t) ~f:snd
  [@@mode p = (nonportable, portable)]
  ;;

  let%template repeat ?(greedy = true) ?(min = 0) ?(max = None) (t : unit t @ p)
    : unit t @ p
    =
    let repeating_re = Re.repn t.regex min max in
    let re =
      match greedy with
      | true -> Re.greedy repeating_re
      | false -> Re.non_greedy repeating_re
    in
    { regex = re
    ; num_captures = t.num_captures
    ; extract_value_from_captured_groups = extract_nothing
    }
  [@@mode p = (nonportable, portable)]
  ;;

  let to_re ?(case_sensitive = true) (t : 'a t) =
    match case_sensitive with
    | true -> Re.case t.regex
    | false -> Re.no_case t.regex
  ;;

  let%template of_re (t : Re.t) : unit t @ p =
    { regex = Re.no_group t
    ; num_captures = 0
    ; extract_value_from_captured_groups = extract_nothing
    }
  [@@mode p = (nonportable, portable)]
  ;;

  let%template or_ (parsers : 'a t list @ p) : 'a t @ p =
    let regex_list =
      (* We add an empty capture group in front of every regex which will help us identify
         which parser was the one that actually matched *)
      List.map parsers ~f:(fun t -> Re.seq [ Re.group Re.epsilon; t.regex ])
    in
    let new_regex = Re.alt regex_list in
    let extract_value_from_captured_groups ~captures ~offset =
      let rec find_the_match (captures : Re.Group.t) (offset : int) (parsers : 'a t list) =
        match parsers with
        | [] ->
          failwith
            "Re.Parser.or_.extract_value_from_captured_groups bug: called on non-match"
        | t :: ts ->
          if Option.is_some (Re.Group.get_opt captures offset)
          then t.extract_value_from_captured_groups ~captures ~offset:(offset + 1)
          else find_the_match captures (offset + 1 + t.num_captures) ts
      in
      find_the_match captures offset parsers
    in
    { regex = new_regex
    ; num_captures = List.sum (module Int) parsers ~f:(fun t -> t.num_captures + 1)
    ; extract_value_from_captured_groups
    }
  [@@mode p = (nonportable, portable)]
  ;;

  let%template times t amt = (repeat [@mode p]) ~min:amt ~max:(Some amt) t
  [@@mode p = (nonportable, portable)]
  ;;

  module Compiled = struct
    type 'a t =
      { rex : Re.re
      ; extract_value_from_captured_groups :
          captures:Re.Group.t -> offset:int -> 'a option
      }
  end

  let%template compile ?(case_sensitive = true) (t : 'a t @ p) : 'a Compiled.t @ p =
    { rex = Re.compile (to_re ~case_sensitive t)
    ; extract_value_from_captured_groups = t.extract_value_from_captured_groups
    }
  [@@mode p = (nonportable, portable)]
  ;;

  let run (compiled : _ Compiled.t) s =
    Re.exec_opt compiled.rex s
    |> Option.bind ~f:(fun captures ->
      compiled.extract_value_from_captured_groups ~captures ~offset:1)
  ;;

  let matches (compiled : _ Compiled.t) input = Re.execp compiled.rex input

  let run_all (compiled : _ Compiled.t) s =
    Re.all compiled.rex s
    |> List.filter_map ~f:(fun captures ->
      compiled.extract_value_from_captured_groups ~captures ~offset:1)
  ;;

  let run_and_split (compiled : _ Compiled.t) s =
    let open Either.Export in
    List.filter_map (Re.split_full compiled.rex s) ~f:(function
      | `Text string -> if String.is_empty string then None else Some (Second string)
      | `Delim captures ->
        (match compiled.extract_value_from_captured_groups ~captures ~offset:1 with
         | Some value -> Some (First value)
         | None ->
           raise_s
             (Sexp.message
                "BUG Re_parser.run_and_split: regex match without parsed value"
                [ "", Atom Stdlib.__LOC__ ])))
  ;;

  let fail =
    { regex = Re.empty
    ; num_captures = 0
    ; extract_value_from_captured_groups =
        (fun ~captures:_ ~offset:_ -> failwith "BUG: This regex should match nothing.")
    }
  ;;

  let%template optional ?(greedy = true) (t : 'a t @ p) : 'a option t @ p =
    let greedy_or_not =
      match greedy with
      | true -> Re.greedy
      | false -> Re.non_greedy
    in
    let regex = t.regex |> Re.group |> Re.opt |> greedy_or_not in
    { regex
    ; num_captures = t.num_captures + 1
    ; extract_value_from_captured_groups =
        (fun ~captures ~offset ->
          match Re.Group.get_opt captures offset with
          | None -> Some None
          | Some _ ->
            Some (t.extract_value_from_captured_groups ~captures ~offset:(offset + 1)))
    }
  [@@mode p = (nonportable, portable)]
  ;;

  module Decimal = struct
    let digit @ portable =
      (map [@mode portable]) Char.digit ~f:(fun c -> Int.of_string (String.of_char c))
    ;;

    let sign @ portable =
      (map [@mode portable])
        ((optional [@mode portable]) (Char.one_of [ '+'; '-' ]))
        ~f:(fun c ->
          match c with
          | None | Some '+' -> 1
          | Some '-' -> -1
          | Some c ->
            raise_s (Atom (Printf.sprintf "matched a character other than [+, -]: %c" c)))
    ;;

    let unsigned @ portable =
      (map [@mode portable])
        ((capture [@mode portable])
           ((repeat [@mode portable]) ~min:1 ((ignore_m [@mode portable]) Char.digit)))
        ~f:Int.of_string
    ;;

    let int @ portable = (map2 [@mode portable]) sign unsigned ~f:( * )
  end
end

include Body

module Open_on_rhs_intf = struct
  module type S = S with type 'a t = 'a t
end

include%template
  Applicative.Make_let_syntax [@modality portable] (Body) (Open_on_rhs_intf) (Body)

module Portable = struct
  module Portable_body : S_portable with type 'a t = 'a t and module Compiled = Compiled =
  struct
    include Body

    let return = (return [@mode portable])
    let apply = (apply [@mode portable])
    let both = (both [@mode portable])
    let map = (map [@mode portable])
    let map2 = (map2 [@mode portable])
    let map3 = (map3 [@mode portable])
    let all = (all [@mode portable])
    let all_unit = (all_unit [@mode portable])
    let ( <*> ) = (( <*> ) [@mode portable])
    let ( <* ) = (( <* ) [@mode portable])
    let ( *> ) = (( *> ) [@mode portable])
    let ( >>| ) = (( >>| ) [@mode portable])

    module%template Applicative_infix = Applicative_infix [@mode portable]

    let compile = (compile [@mode portable])
    let ignore_m = (ignore_m [@mode portable])
    let capture = (capture [@mode portable])
    let and_capture = (and_capture [@mode portable])
    let or_ = (or_ [@mode portable])
    let optional = (optional [@mode portable])
    let repeat = (repeat [@mode portable])
    let times = (times [@mode portable])
    let of_re = (of_re [@mode portable])
  end

  include Portable_body

  module Open_on_rhs_intf = struct
    module type S = S_portable with type 'a t = 'a t
  end

  module Let_syntax = struct
    include Portable_body

    module Let_syntax = struct
      include Portable_body
      module Open_on_rhs = Portable_body
    end
  end
end
