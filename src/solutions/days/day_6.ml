open Solvers.Signature
let remove_first = function
  | _ :: t -> t
  | [] -> []

module Solver : Solver = struct
  let parse input =
    let get_numbers x = List.map float_of_string (remove_first (Str.split (Str.regexp " +") x)) in
    List.map get_numbers input
  let smaller_integer x = let y = float_of_int (int_of_float x) in if x = y then (int_of_float x) - 1 else (int_of_float x)
  let find_zeroes t x  n_sol = ((-.t +. n_sol *. sqrt(t *. t -. 4.0 *. x)) /. (-2.))
  let task1 data =
    let info = parse (String.split_on_char '\n' data ) in
    let find_sol time max = List.map2 (fun t x -> (smaller_integer (find_zeroes t x (-.1.)) - int_of_float ((find_zeroes t x 1.) +. 1.))+1) time max in
    string_of_int (List.fold_left ( * ) 1 (find_sol (List.nth info 0) (List.nth info 1)))
  let parse2 input =
    let get_number x = float_of_string (List.nth (String.split_on_char ':' (Str.global_replace (Str.regexp " ") "" x)) 1) in
    List.map get_number input
  let task2 data _part1 =
    let info = parse2 (String.split_on_char '\n' data ) in
    let find_sol t x = smaller_integer (find_zeroes t x (-.1.)) - int_of_float ((find_zeroes t x 1.) +. 1.)+1 in
    string_of_int (find_sol (List.nth info 0) (List.nth info 1))
end;;