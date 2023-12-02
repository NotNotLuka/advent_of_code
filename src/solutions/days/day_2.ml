open Solvers.Signature
open Utils.String_module

type showcase = {
  red : int;
  green : int;
  blue : int;
}

let max_showcase = {red=12;green=13;blue=14}

type game_info = {
  game_n : int;
  possible : bool;
}

module Solver : Solver = struct
  let rec game_info lines acc =
    let rec check_specific data =
      match data with
      | [] -> true
      | h :: t -> let colour = split ' ' (String.trim h) in 
                  match String.trim (List.nth colour 1) with
                  | "red" -> if int_of_string (List.nth colour 0) <= max_showcase.red 
                            then check_specific t 
                            else false
                  | "blue" -> if int_of_string (List.nth colour 0) <= max_showcase.blue 
                              then check_specific t 
                              else false
                  | "green" -> if int_of_string (List.nth colour 0) <= max_showcase.green 
                              then check_specific t 
                              else false
                  | s -> failwith ("unknown colour " ^ s) in
    let rec check_all data =
      match data with
      | [] -> true
      | h :: t -> if not (check_specific (split ',' h)) 
                  then false
                  else check_all t in
    match lines with
    | [] -> acc
    | h :: t -> let info = split ':' h in 
                if check_all (split ';' (List.nth info 1)) 
                then game_info t ((int_of_string (List.nth (split ' ' (List.hd info)) 1 )) + acc)
                else game_info t acc
  let task1 data =
    let all_lines = (split '\n' data) in
    string_of_int (game_info all_lines 0)

    let rec check_each_minimum data minimum_cubes =
      match data with
      | [] -> minimum_cubes
      | h :: t -> let colour = split ' ' (String.trim h) in 
                  let n = int_of_string (List.nth colour 0) in 
                  match String.trim (List.nth colour 1) with
                  | "red" -> if n <= minimum_cubes.red 
                            then check_each_minimum t minimum_cubes 
                            else check_each_minimum t {minimum_cubes with red=n}
                  | "blue" -> if n <= minimum_cubes.blue 
                            then check_each_minimum t minimum_cubes 
                            else check_each_minimum t {minimum_cubes with blue=n}
                  | "green" -> if n <= minimum_cubes.green 
                              then check_each_minimum t minimum_cubes 
                              else check_each_minimum t {minimum_cubes with green=n}
                  | s -> failwith ("unknown colour " ^ s) 
    let rec get_product data acc =
      match data with
      | [] -> acc.red * acc.blue * acc.green
      | h :: t -> get_product t (check_each_minimum (split ',' h) acc)
    let rec get_sum lines acc =
      match lines with
      | [] -> acc
      | h :: t -> let info = split ':' h in 
                  get_sum t ((get_product (split ';' (List.nth info 1)) {red=0;blue=0;green=0}) + acc)
  let task2 data _part1 =

    let all_lines = (split '\n' data) in
    string_of_int (get_sum all_lines 0)
end;;