open Solvers.Signature


module Solver : Solver = struct
  let get_diff lst =
    let rec aux prev lst acc =
      match lst with
      | [] -> acc
      | h :: t -> aux h t ((h - prev) :: acc) in 
    aux (List.hd lst) (List.tl lst) []
  
  let construct_derivatives lst =
    let rec aux lst acc =
      let diff = List.rev (get_diff lst) in
      if (List.filter (fun x -> x <> 0) diff) = [] then acc
      else aux diff ((diff) :: acc) in
    aux lst [lst] 
  
  let next_value lst =
    let derivatives = construct_derivatives lst in
    List.fold_left (fun acc x -> acc + (List.hd (List.rev x))) 0 derivatives

  let task1 data =
    let parsed = List.map (fun x-> List.map int_of_string (String.split_on_char ' ' x)) (String.split_on_char '\n' data) in 
    string_of_int (List.fold_left (+) 0 (List.map next_value parsed))

  let first_value lst =
    let derivatives = construct_derivatives lst in
    let rec aux prev lst =
      match lst with
      | [] -> prev
      | h :: t -> aux ((List.hd h) - prev) t in
    aux (List.hd (List.hd derivatives)) (List.tl derivatives)

  let task2 data _part1 =
    let parsed = List.map (fun x-> List.map int_of_string (String.split_on_char ' ' x)) (String.split_on_char '\n' data) in 
    string_of_int (List.fold_left (+) 0 (List.map first_value parsed))
end;;