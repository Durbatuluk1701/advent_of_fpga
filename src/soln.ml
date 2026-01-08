open! Core
open! Hardcaml

module type Solution = sig
  val name : string
  val num_bits : int

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; finish : 'a
      ; data_in : 'a
      ; data_in_valid : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { max_range : 'a With_valid.t
      ; min_range : 'a With_valid.t
      }
    [@@deriving hardcaml]
  end

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end

let soln_safe_name (name : string) : string =
  String.map name ~f:(function
    | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c -> c
    | _ -> '_')
;;
