open Solvers.Signature
open Utils.String_module
open Utils.List_module


module Solver : Solver = struct
  let rec find_empty map (x, y) (x_inc, y_inc) = 
    if y < List.length map && x < List.length (List.hd map) then
      if List.nth (List.nth map y) x = '.' 
      then find_empty map (x + x_inc, y + y_inc) (x_inc, y_inc)
      else false
    else true
  
  let rec go_through map y acc=
    if y < List.length map then
      if find_empty map (0, y) (1, 0)
      then go_through map (y + 1) (y :: acc)
      else go_through map (y + 1) (acc)
    else acc 

  let parse input = List.map stringToList (Str.split (Str.regexp "\n") input)

  let find_galaxies map n =
    let x_increase = go_through (transpose map) 0 [] in
    let y_increase = go_through map 0 [] in

    let rec aux (x, y) (x_act, y_act) acc =
      let y_inc = if List.mem y y_increase then n else 1 in
      if y < (List.length map) then
        if x < (List.length (List.hd map)) then
          let x_inc = if List.mem x x_increase then n else 1 in
          if List.nth (List.nth map y) x = '#' 
          then aux ((x + 1), y) (x_act + x_inc, y_act) ((x_act, y_act) :: acc)
          else aux ((x + 1), y) (x_act + x_inc, y_act) acc
        else aux (0, (y + 1)) (0, y_act + y_inc) acc
      else acc
    in
    aux (0, 0) (0, 0) []
  
  let get_distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

  let sum_distances galaxies =
    let rec aux galaxies acc =
      match galaxies with
      | [] -> acc
      | h :: t -> 
        aux t (List.fold_left (fun acc x -> (get_distance h x) + acc) acc t) in 
    aux galaxies 0

  let task1 data =
    let parsed =  parse data  in
    let galaxies = find_galaxies parsed 2 in 
    string_of_int (sum_distances galaxies)
  
  let task2 data _part1 =
    let parsed =  parse data  in
    let galaxies = find_galaxies parsed 1000000 in 
    string_of_int (sum_distances galaxies)
end;;