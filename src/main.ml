open Solvers.Signature
open Fileutils.File_module

let choose_solver day : (module Solver) =
    let open Solutions in
    match day with
    | "1" -> (module Day_1.Solver)
    | _ -> failwith "Ni še rešeno"

let main () =
  Printexc.record_backtrace true;
  let day = Sys.argv.(1) in
  print_endline ("Solving DAY: " ^ day);
  let (module Solver) = choose_solver day in
  let input_data =
    Fileutils.File_module.read_file ("data/day_" ^ day ^ ".in")
  in
  let p1_start = Sys.time () in
  let part1 = Solver.naloga1 input_data in
  let t1_time = Sys.time () -. p1_start in
  print_endline "PART 1:";
  print_endline part1;
  print_endline ("Taken: " ^ string_of_float t1_time ^ "s");
  let p2_start = Sys.time () in
  let part2 = Solver.naloga2 input_data part1 in
  let t2_time = Sys.time () -. p2_start in
  print_endline "PART 2:";
  print_endline part2;
  print_endline ("Taken: " ^ string_of_float t2_time ^ "s");
  print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");
  write_to_file ("out/day_" ^ day ^ "_1.out") part1;
  write_to_file ("out/day_" ^ day ^ "_2.out") part2;
  ()
    

let _ = main ()