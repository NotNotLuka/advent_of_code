open Solvers.Signature
open Utils.String_module
(* open Utils.Print_module *)
open Utils.List_module

module Solver : Solver = struct
  let parse input = 
    let maps = Str.split (Str.regexp "\n\n") input in
    List.map (fun map -> (List.map stringToList (Str.split (Str.regexp "\n") map))) maps
  
  let rec compare a b diff =
    match a, b with
    | [], [] -> diff
    | h1 :: t1, h2 :: t2 -> if h1 = h2 then compare t1 t2 diff else compare t1 t2 (diff+1)
    | _ -> failwith "Invalid input"
  let rec check_mirror map x flipped original_map acceptable =
    let rec check_y y diff =
      if y < List.length map 
      then 
          let left = (slice (List.nth map y) 0 x) in 
          let right = (slice (List.nth map y) (x) (x+x)) in
          let difference = compare (List.rev left) right diff in
          if difference < 2
          then check_y (y+1) difference
          else false
      else if diff = acceptable then true else false
      in
    if check_y 0 0
    then 
      begin
      if flipped then (List.length (List.nth map 0)) - x else x
      end
    else 
      if x < (List.length (List.nth map 0)) / 2
      then check_mirror map (x+1) flipped original_map acceptable
      else if not flipped
        then 
        check_mirror (List.map (fun line -> List.rev line) map) 1 true original_map acceptable
        else (check_mirror (transpose original_map) 1 false original_map acceptable) * 100
  let task1 data =
    let maps = parse data in
    let x = List.map (fun map -> check_mirror map 1 false map 0) maps in
    string_of_int (List.fold_left (+) 0 x)
  

  let task2 data _part1 =
    let maps = parse data in
    let x = List.map (fun map -> check_mirror map 1 false map 1) maps in
    string_of_int (List.fold_left (+) 0 x)
end;;