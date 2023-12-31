open Solvers.Signature
open Utils.File_module

let choose_solver day : (module Solver) =
    let open Solutions in
    match day with
    | "1" -> (module Day_1.Solver)
    | "2" -> (module Day_2.Solver)
    | "3" -> (module Day_3.Solver)
    | "4" -> (module Day_4.Solver)
    | "5" -> (module Day_5.Solver)
    | "6" -> (module Day_6.Solver)
    | "7" -> (module Day_7.Solver)
    | "8" -> (module Day_8.Solver)
    | "9" -> (module Day_9.Solver)
    | "10" -> (module Day_10.Solver)
    | "11" -> (module Day_11.Solver)
    | "12" -> (module Day_12.Solver)
    | "13" -> (module Day_13.Solver)
    | "14" -> (module Day_14.Solver)
    | "15" -> (module Day_15.Solver)
    | "16" -> (module Day_16.Solver)
    | _ -> failwith "Ni še rešeno"

let main () =
  Printexc.record_backtrace true;
  let day = Sys.argv.(1) in
  print_endline ("Solving DAY: " ^ day);
  let (module Solver) = choose_solver day in
  let input_data =
    read_file ("data/day_" ^ day ^ ".in")
  in
  let p1_start = Unix.gettimeofday() in
  let part1 = Solver.task1 input_data in
  let t1_time = Unix.gettimeofday() -. p1_start in
  print_endline "PART 1:";
  print_endline part1;
  print_endline ("Taken: " ^ string_of_float t1_time ^ "s");
  let p2_start = Sys.time () in
  let part2 = Solver.task2 input_data part1 in
  let t2_time = Sys.time () -. p2_start in
  print_endline "PART 2:";
  print_endline part2;
  print_endline ("Taken: " ^ string_of_float t2_time ^ "s");
  print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");
  write_to_file ("out/day_" ^ day ^ "_1.out") part1;
  write_to_file ("out/day_" ^ day ^ "_2.out") part2;
  ()
    

let _ = main ()