open! Core
open! Hardcaml

module type Solution = sig
  val name : string

  module I : Hardcaml.Interface.S
  module O : Hardcaml.Interface.S

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end

let soln_safe_name (name : string) : string =
  String.map name ~f:(function
    | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c -> c
    | _ -> '_')
;;
