open Core
open Hardcaml
open Hardcaml_circuits
open Signal

(* System Parameters *)
let start_pos = 50
let dial_size = 100

(** [2**mag_bits - 1] is the maximum magnitude of a single turn *)
let mag_bits = 16

(*
   The position bits need to be sufficient to represent the dial size:
    Thus, we need at least ceil(log2(dial_size)) bits
    2^log2(dial_size) = dial_size ==> log2(dial_size) = number of bits needed
    Round to nearest integer using ceil
*)
let pos_bits = Int.ceil_log2 dial_size

(* We need to know pos_bits <= mag_bits so zero extension is valid *)
let () = assert (pos_bits <= mag_bits)

(** [2**count_bits - 1] is the maximum count value that can occur *)
let count_bits = 32

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; direction : 'a [@rtlname "dir"] (* '0' = Left, '1' = Right *)
    ; magnitude : 'a [@bits mag_bits]
    ; data_in_valid : 'a
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { count_on_zero : 'a With_valid.t [@bits count_bits]
    ; count_pass_zero : 'a With_valid.t [@bits count_bits]
    ; position : 'a With_valid.t [@bits pos_bits]
    }
  [@@deriving sexp_of, hardcaml]
end

module States = struct
  type t =
    | Idle
    | Updating_positions
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

module DivByCnst = Divide_by_constant.Make (Signal)

let create
  scope
  ({ clock; clear; start; finish; direction; magnitude; data_in_valid } : _ I.t)
  : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  let pos_dial_size = Signal.of_int_trunc ~width:pos_bits dial_size in
  let%hw_var position = Variable.reg spec ~width:pos_bits in
  let%hw_var count_on_zero = Variable.reg spec ~width:count_bits in
  let%hw_var count_pass_zero = Variable.reg spec ~width:count_bits in
  let valid_count = Variable.wire ~default:gnd () in
  compile
    [ sm.switch
        [ ( Idle
          , [ when_
                start
                [ position <-- Signal.of_int_trunc ~width:pos_bits start_pos
                ; count_on_zero <-- zero count_bits
                ; count_pass_zero <-- zero count_bits
                ; sm.set_next Updating_positions
                ]
            ] )
        ; ( Updating_positions
          , let real_mag =
              mux2
                (direction ==:. 0)
                magnitude
                (Signal.uresize ~width:mag_bits position.value +: magnitude)
            in
            let mag_q =
              DivByCnst.divide ~divisor:(Bigint.of_int dial_size) real_mag
              |> Signal.uresize ~width:mag_bits
            in
            let tmp_mag_r =
              mag_q *: Signal.of_int_trunc ~width:mag_bits dial_size
              |> Signal.uresize ~width:mag_bits
            in
            let mag_r = real_mag -: tmp_mag_r |> Signal.uresize ~width:pos_bits in
            let num_twists = Signal.uresize ~width:count_bits mag_q in
            [ when_
                data_in_valid
                [ if_
                    (direction ==:. 0)
                    [ if_
                        (position.value <: mag_r)
                        [ if_
                            (position.value ==:. 0)
                            (* if the position start in 0, then it is NOT an extra passing of 0 *)
                            [ count_pass_zero <-- count_pass_zero.value +: num_twists ]
                            [ count_pass_zero
                              <-- count_pass_zero.value +: num_twists +: one count_bits
                            ]
                          (* (count_pass_zero
                           <-- count_pass_zero.value
                               +: num_twists
                               +:
                               if position.value ==:. 0
                               then one count_bits
                               else zero count_bits) *)
                        ; position <-- pos_dial_size -: mag_r +: position.value
                        ; when_
                            (pos_dial_size -: mag_r +: position.value ==:. 0)
                            [ incr count_on_zero ]
                        ]
                        [ if_
                            (position.value ==: mag_r)
                            [ count_pass_zero
                              <-- count_pass_zero.value +: num_twists +: one count_bits
                            ; position <-- position.value -: mag_r
                            ; incr count_on_zero
                            ]
                            [ count_pass_zero <-- count_pass_zero.value +: num_twists
                            ; position <-- position.value -: mag_r
                            ; when_
                                (position.value -: mag_r ==:. 0)
                                [ incr count_on_zero ]
                            ]
                        ]
                    ]
                    [ (* direction == 1 *)
                      count_pass_zero <-- count_pass_zero.value +: num_twists
                    ; position <-- mag_r
                    ; when_ (mag_r ==:. 0) [ incr count_on_zero ]
                    ]
                ]
            ; when_ finish [ sm.set_next Done ]
            ] )
        ; Done, [ valid_count <-- vdd; when_ finish [ sm.set_next Updating_positions ] ]
        ]
    ];
  { O.count_on_zero = { value = count_on_zero.value; valid = valid_count.value }
  ; count_pass_zero = { value = count_pass_zero.value; valid = valid_count.value }
  ; position = { value = position.value; valid = valid_count.value }
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day1" create
;;
