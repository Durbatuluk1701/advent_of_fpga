open! Core
open! Hardcaml
open! Advent_of_fpga

let generate_rtl (module Sln : Soln.Solution) =
  let module C = Circuit.With_interface (Sln.I) (Sln.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit =
    C.create_exn ~name:(Soln.soln_safe_name Sln.name) (Sln.hierarchical scope)
  in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  print_endline rtl
;;

let soln_rtl_command (module Sln : Soln.Solution) =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () -> generate_rtl (module Sln)]
;;

let solns = [ (module Day1 : Soln.Solution) ]

let () =
  match Sys.get_argv () with
  | [| _; target |] ->
    (match List.find solns ~f:(fun (module Sln) -> String.equal Sln.name target) with
     | None ->
       eprintf "Unknown solution name: %s\n" target;
       exit 1
     | Some (module Sln) ->
       Command_unix.run
         (Command.group ~summary:"" [ Sln.name, soln_rtl_command (module Sln) ]))
  | _ ->
    let valid_solns = List.map solns ~f:(fun (module Sln) -> Sln.name) in
    eprintf
      "Usage: %s <solution-name>\n\n%s\n"
      Sys_unix.executable_name
      ("Valid solutions are:\n"
       ^ String.concat ~sep:"\n" (List.map valid_solns ~f:(sprintf "- %s")));
    exit 1
;;
