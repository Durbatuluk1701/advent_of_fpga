open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Day1 = Hardcaml_demo_project.Day1
module Harness = Cyclesim_harness.Make (Day1.I) (Day1.O)

let ( <--. ) = Bits.( <--. )

type entry =
  | Left of int
  | Right of int

let example_file = "./inputs/day1_example.txt"
let test_file = "./inputs/day1_test.txt"

let parse_input_file filename =
  In_channel.read_lines filename
  |> List.map ~f:(fun s ->
    let mag = Int.of_string (String.sub s ~pos:1 ~len:(String.length s - 1)) in
    match s.[0] with
    | 'L' -> Left mag
    | 'R' -> Right mag
    | _ -> failwith "Invalid direction in input file")
;;

(* let pp_entry = function
  | Left mag -> sprintf "L%i" mag
  | Right mag -> sprintf "R%i" mag
;; *)

let sample_input_values =
  [ 0, 68; 0, 30; 1, 48; 0, 5; 1, 60; 0, 55; 0, 1; 0, 99; 1, 14; 0, 82 ]
;;

let simple_testbench test_file (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  (* Helper function for inputting one value *)
  let feed_input entry =
    (* let prev_pos = Bits.to_unsigned_int !(outputs.position.value) in *)
    let mag =
      match entry with
      | Left mag ->
        inputs.direction := Bits.gnd;
        mag
      | Right mag ->
        inputs.direction := Bits.vdd;
        mag
    in
    inputs.magnitude <--. mag;
    cycle ()
    (* printf
      "%i \t=[ %s \t(%i) ]=>\n"
      prev_pos
      (pp_entry entry)
      (Bits.to_unsigned_int !(outputs.count_pass_zero.value)) *)
  in
  (* Reset the design *)
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  cycle ();
  (* Pulse the start signal *)
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;
  (* Input some data *)
  inputs.data_in_valid := Bits.vdd;
  List.iter (parse_input_file test_file) ~f:(fun x -> feed_input x);
  inputs.data_in_valid := Bits.gnd;
  cycle ();
  inputs.finish := Bits.vdd;
  cycle ();
  inputs.finish := Bits.gnd;
  cycle ();
  (* Wait for result to become valid *)
  while not (Bits.to_bool !(outputs.count_pass_zero.valid)) do
    cycle ()
  done;
  let count_pass_zero = Bits.to_unsigned_int !(outputs.count_pass_zero.value) in
  let count_on_zero = Bits.to_unsigned_int !(outputs.count_on_zero.value) in
  let position = Bits.to_unsigned_int !(outputs.position.value) in
  print_s
    [%message "Result" (count_pass_zero : int) (count_on_zero : int) (position : int)];
  cycle ()
;;

(* The [waves_config] argument to [Harness.run] determines where and how to save waveforms
   for viewing later with a waveform viewer. The commented examples below show how to save
   a waveterm file or a VCD file. *)
(* let waves_config = Waves_config.no_waves *)

let waves_config =
  Waves_config.to_directory "/tmp/" |> Waves_config.as_wavefile_format ~format:Vcd
;;

let%expect_test "Day1 Example Problem Test: Saves Waveform to tmp" =
  Harness.run_advanced
    ~waves_config
    ~create:Day1.hierarchical
    (simple_testbench example_file);
  [%expect
    {|
    (Result (count_pass_zero 6) (count_on_zero 3) (position 32))
    Saved waves to /tmp/test_day1_ml_Day1_Example_Problem_Test__Saves_Waveform_to_tmp.vcd
    |}]
;;

let%expect_test "Day1 Test Problem Test: Saves Waveform to tmp" =
  Harness.run_advanced
    ~waves_config
    ~create:Day1.hierarchical
    (simple_testbench test_file);
  [%expect
    {|
    (Result (count_pass_zero 5820) (count_on_zero 1007) (position 4))
    Saved waves to /tmp/test_day1_ml_Day1_Test_Problem_Test__Saves_Waveform_to_tmp.vcd
    |}]
;;
