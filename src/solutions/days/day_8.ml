open Solvers.Signature
open Stdlib
open Utils.String_module

module StringMap = Map.Make(String)

type cycle = {
  start : int;
  all_zeds : int list;
}

module Solver : Solver = struct
  let parse input =
    let lines = String.split_on_char '\n' input in
    let instruct = Str.global_replace (Str.regexp "R") "1" (List.hd lines) in 
    let instructions = List.map (fun x ->(int_of_char x) - 48) (stringToList (Str.global_replace (Str.regexp "L") "0" instruct)) in
    let map = List.tl (List.tl lines) in
    let filter string = Str.global_replace (Str.regexp "[() ]") "" string in
    let split_line acc line  = 
      let splitted = Str.split (Str.regexp " = ") line in
      let key = List.nth splitted 0 in
      let value = String.split_on_char ',' (filter (List.nth splitted 1)) in
      StringMap.add key value acc in
    let x = List.fold_left split_line StringMap.empty map in
    instructions, x

  let get_final_position (instructions, map) =
    let rec aux pos instructions map intact_instructions n =
      match instructions with
      | [] -> aux pos intact_instructions map intact_instructions n
      | h :: t -> let new_pos = List.nth (StringMap.find pos map) h in
                  if new_pos = "ZZZ" then n
                  else aux new_pos t map intact_instructions (n+1) in 

    aux "AAA" instructions map instructions 1
  let task1 data =
    let parsed = parse data in
    string_of_int (get_final_position parsed)
  let get_final_position (instructions, map) =
    let get_starting_poss map =
      StringMap.fold (fun key _ acc -> if key.[2] = 'A' then key :: acc else acc) map [] in

    let lcm list =
      (* Source: https://www.geeksforgeeks.org/lcm-of-given-array-elements/ *)
      let gcd a b =
        let rec aux a b =
          if b = 0 then a else aux b (a mod b) in
        aux a b in
      let lcm = List.hd list in
      let rec aux list lcm =
        match list with
        | [] -> lcm
        | h :: t -> aux t ((lcm * h) / (gcd lcm h)) in
      aux (List.tl list) lcm in

    let rec aux instructions map intact_instructions n pos =
      match instructions with
      | [] -> aux intact_instructions map intact_instructions n pos
      | h :: t -> let new_pos = List.nth (StringMap.find pos map) h in
                  if new_pos.[2] = 'Z' then n
                  else aux t map intact_instructions (n+1) new_pos in 
    
    let starting_pos = get_starting_poss map in
    let cycle_length = (List.map (aux instructions map instructions 1) starting_pos) in
    lcm cycle_length
  let task2 data _part1 =
    let parsed = parse data in
    string_of_int (get_final_position parsed)
end;;